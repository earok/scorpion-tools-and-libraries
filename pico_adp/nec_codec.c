/*
	Encode and decode algorithms for
	NEC uPD775x ADPCM

	2022 by superctr.
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

// Currently assume that a silence block is 128 cycles (at 640khz/4) like jotego's FPGA core
// MAME seems to be set at 256 if I read correctly. Needs to be verified.
#define NEC_SILENCE_LEN 128.0

#define CLAMP(x, low, high)  (((x) > (high)) ? (high) : (((x) < (low)) ? (low) : (x)))

static const int16_t nec_step_table[256] = {
	0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 2, 2, 3, 4, 4, 6,
	0, 1, 1, 1, 2, 2, 3, 4, 4, 6, 7, 9, 11, 13, 16, 20,
	1, 2, 2, 3, 3, 4, 5, 7, 8, 10, 12, 16, 19, 24, 29, 36,
	2, 3, 4, 4, 5, 7, 8, 10, 13, 16, 19, 24, 29, 36, 44, 54,
	3, 4, 5, 6, 8, 10, 12, 15, 18, 22, 27, 34, 41, 50, 62, 76,
	5, 6, 7, 9, 11, 14, 16, 20, 25, 31, 37, 46, 57, 69, 85, 104,
	7, 8, 10, 13, 15, 19, 22, 29, 35, 43, 51, 64, 79, 96, 118, 144,
	10, 13, 15, 19, 23, 29, 33, 43, 53, 64, 76, 96, 117, 143, 175, 214,

	-0, -0, -0, -0, -0, -0, -0, -1, -1, -1, -2, -2, -3, -4, -4, -6,
	-0, -1, -1, -1, -2, -2, -3, -4, -4, -6, -7, -9, -11, -13, -16, -20,
	-1, -2, -2, -3, -3, -4, -5, -7, -8, -10, -12, -16, -19, -24, -29, -36,
	-2, -3, -4, -4, -5, -7, -8, -10, -13, -16, -19, -24, -29, -36, -44, -54,
	-3, -4, -5, -6, -8, -10, -12, -15, -18, -22, -27, -34, -41, -50, -62, -76,
	-5, -6, -7, -9, -11, -14, -16, -20, -25, -31, -37, -46, -57, -69, -85, -104,
	-7, -8, -10, -13, -15, -19, -22, -29, -35, -43, -51, -64, -79, -96, -118, -144,
	-10, -13, -15, -19, -23, -29, -33, -43, -53, -64, -76, -96, -117, -143, -175, -214
};

static const int8_t nec_adjust_table[16] = {
	-1, -1, 0, 0, 1, 2, 2, 3, -1, -1, 0, 0, 1, 2, 2, 3
};

// the MSB nibble of the step is the sample. LSB nibble must be 0
static inline int16_t nec_step(uint8_t step, int16_t* history, int8_t* step_hist)
{
	int16_t out = *history + nec_step_table[step + *step_hist];
	int8_t next_hist = *step_hist + nec_adjust_table[step >> 4];
	out = CLAMP(out, -256, 255);
	*history = out;
	*step_hist = CLAMP(next_hist, 0, 15);

	return out;
}

// the MSB nibble of the step is the sample
static inline uint8_t nec_encode_step(int16_t input, int16_t* history, int8_t* step_hist)
{
	int16_t delta = input - *history;
	uint8_t sign = (delta < 0) ? 0x80 : 0;

	delta = abs(delta);

	uint8_t step = *step_hist;
	uint16_t best_delta = abs(nec_step_table[step] - delta);
	while(best_delta && step < 0x70)
	{
		uint16_t next_delta = abs(nec_step_table[step + 0x10] - delta);
		if(next_delta > best_delta)
			break;
		best_delta = next_delta;
		step += 0x10;
	}

	step = (step + sign) & 0xf0;
	nec_step(step, history, step_hist);

	return step;
}

// Encode raw sample data (Without block information)
void nec_encode_raw(int16_t *buffer,uint8_t *outbuffer,long len)
{
	long i;

	int16_t history = 0;
	int8_t step_hist = 0;
	uint8_t buf_sample = 0, nibble = 0;

	for(i=0;i<len;i++)
	{
		int16_t sample = *buffer++;
		if(sample < 0x7fc0) // round up
			sample += 64;
		sample >>= 7;
		uint8_t step = nec_encode_step(sample, &history, &step_hist);
		if(nibble)
			*outbuffer++ = buf_sample | (step >> 4);
		else
			buf_sample = step;
		nibble^=1;
	}
}

void nec_decode_raw(uint8_t *buffer,int16_t *outbuffer,long len)
{
	long i;

	int16_t history = 0;
	int8_t step_hist = 0;
	uint8_t nibble = 0;

	for(i=0;i<len;i++)
	{
		uint8_t step = *buffer << nibble;
		if(nibble)
			buffer++;
		nibble^=4;
		*outbuffer++ = nec_step(step & 0xf0, &history, &step_hist);
	}
}

//==============================


// Get the number of silence blocks
static int detect_silence(int16_t *buffer, long len, double silence_len, int16_t threshold)
{
	long i;
	for(i = 0; i < len; i++)
	{
		if(abs(*buffer++) >= threshold)
			break;
	}
	return i / silence_len;
}

// Add silence blocks
static uint8_t* add_silence(uint8_t *outbuffer, int count)
{
	while(count > 64)
	{
		*outbuffer++ = 0x3f;
		count -= 64;
	}
	if(count > 1)
	{
		*outbuffer++ = count - 1;
	}
	return outbuffer;
}

// Add sample blocks
static uint8_t* add_samples(uint8_t *outbuffer, uint8_t *nibble_buf, int count, uint8_t rate)
{
	while(count > 256)
	{
		uint8_t nibble = 0;
		long len = 256;

		*outbuffer++ = 0x40 + rate - 1;
		count -= 256;
		while(len--)
		{
			uint8_t step = *nibble_buf++;
			if(nibble)
				*(outbuffer - 1) |= (step >> 4);
			else
				*outbuffer++ = step & 0xf0;
			nibble^=1;
		}

	}
	if(count)
	{
		uint8_t nibble = 0;

		*outbuffer++ = 0x80 + rate - 1;
		*outbuffer++ = count - 1;

		while(count--)
		{
			uint8_t step = *nibble_buf++;
			if(nibble)
				*(outbuffer - 1) |= (step >> 4);
			else
				*outbuffer++ = step & 0xf0;
			nibble^=1;
		}
	}

	return outbuffer;
}

// Encode block format
__declspec(dllexport) uint8_t* nec_encode(int16_t *buffer,uint8_t *outbuffer,long len,uint8_t div,int16_t threshold,int master)
{
	// Duration of the silence block is independent from the ADPCM sampling rate
	// TODO: Does the silence time apply for Pico too?
	double silence_len = NEC_SILENCE_LEN / div;

	long head_pos = 0, tail_pos = 0, silence_blocks = 0;

	// ADPCM state
	int16_t history = 0;
	int8_t step_hist = 0;

	// Starting out buffer
	uint8_t *outbuffer_start = outbuffer;
	// Temporary buffer for repeat detection
	uint8_t *nibble_buf = malloc(len * sizeof(uint8_t));
	if(!nibble_buf)
		return NULL;

	// Minimum threshold for silence detection
	if(threshold < 32)
		threshold = 31;

	// Check for initial silence
	silence_blocks = detect_silence(buffer, len, silence_len, threshold);
	tail_pos += silence_blocks * silence_len;

	if(!silence_blocks) // Always add at least 1 silence block in order to reset ADPCM state
		silence_blocks++;

	printf("%d initial silence blocks\n", silence_blocks);

	outbuffer = add_silence(outbuffer, silence_blocks);
	head_pos = tail_pos;

	// Main encoding loop
	while(head_pos < len)
	{
		// Get position of next silence
		while(tail_pos < len)
		{
			silence_blocks = detect_silence(buffer + tail_pos, len - tail_pos, silence_len, threshold);
			if(silence_blocks < 2)
				tail_pos++;
			else
				break;
		}

		printf("Encoding %d samples\n", tail_pos - head_pos);

		uint8_t *nibble_tail = nibble_buf;

		// Encode to the nibble buffer
		while(head_pos < tail_pos)
		{
			int16_t sample = buffer[head_pos++];
			if(sample < 0x7fc0) // round up
				sample += 64;
			sample >>= 7;
			*nibble_tail++ = nec_encode_step(sample, &history, &step_hist);
		}

		//TODO: repeating sample blocks
		if(master)
		{
			printf("Master mode not yet implemented!!!\n");
			master = 0;

			outbuffer = add_samples(outbuffer, nibble_buf, nibble_tail - nibble_buf, div);
		}
		else
		{
			outbuffer = add_samples(outbuffer, nibble_buf, nibble_tail - nibble_buf, div);
		}

		if(silence_blocks > 1)
		{
			printf("Adding %d silence blocks\n", silence_blocks);

			// Clear ADPCM state
			history = 0;
			step_hist = 0;

			outbuffer = add_silence(outbuffer, silence_blocks);

			tail_pos += silence_blocks * silence_len;
		}
		head_pos = tail_pos;
	}
	*outbuffer++ = 0x00;

	free(nibble_buf);
	return outbuffer;
}

// Pre pass to get the optimal divider value
__declspec(dllexport) uint8_t nec_get_divider(uint8_t *buffer, long len)
{
	int div = 0;
	int valid_header = 0;

	long pos = 0;
	while(pos < len)
	{
		//printf("pos = %08x, div=%d\n",pos,div);
		uint16_t nibbles;
		uint8_t byte = buffer[pos++];
		// Silence block
		if((byte & 0xc0) == 0)
		{
			if(!byte && valid_header)
				break;
			if(byte)
				valid_header = 1;
		}
		else
		{
			valid_header = 1;
			// Repeat block
			if((byte & 0xc0) == 0xc0)
			{
				printf("Repeat block detected at %08x value %02x\n",pos,byte);
				byte = buffer[pos++];
				//TODO: need to investigate if this is possible
				if((byte & 0xc0) == 0x00)
				{
					printf("Repeating silence block detected at %08x - not supported\n",pos);
					return 0;
				}
			}
			// Safety...
			if(pos >= len)
				return 0;
			// Read divider
			if(!div)
				div = (byte & 0x3f) + 1;
			else if(((byte & 0x3f) + 1) != div)
				div = 1;
			// Read # of nibbles
			if ((byte & 0xc0) == 0x80) // Skip length byte
				nibbles = buffer[pos++] + 1;
			else
				nibbles = 256;
			pos += (nibbles + 1) >> 1;
			// Safety...
			if(pos > len)
				return 0;
		}
	}
	return div;
}

// Pre pass to get the necessary buffer size
__declspec(dllexport) long nec_get_length(uint8_t *buffer, long len, uint8_t div)
{
	double silence_len = NEC_SILENCE_LEN / div;
	long buflen = 0;

	int valid_header = 0;

	long pos = 0;
	while(pos < len)
	{
		//printf("pos = %08x, buflen=%d\n",pos,buflen);
		uint16_t nibbles;
		uint8_t byte = buffer[pos++];
		// Silence block
		if((byte & 0xc0) == 0)
		{
			if(!byte && valid_header)
				break;
			if(byte)
				valid_header = 1;

			buflen += silence_len * ((byte & 0x3f) + 1);
		}
		else
		{
			int repeat_count = 1;
			uint8_t block_div = 1;

			valid_header = 1;

			// Repeat block
			if((byte & 0xc0) == 0xc0)
			{
				repeat_count = (byte & 7) + 2;
				byte = buffer[pos++];
			}
			// Read divider
			block_div = (byte & 0x3f) + 1;
			// Read # of nibbles
			if ((byte & 0xc0) == 0x80) // Skip length byte
				nibbles = buffer[pos++] + 1;
			else
				nibbles = 256;

			pos += (nibbles + 1) >> 1;
			buflen += (block_div / div) * nibbles * repeat_count;
		}
	}
	return buflen;
}

static uint16_t* decode_block(uint8_t *buffer, int16_t *outbuffer, int16_t *history, int8_t *step_hist, long nibbles, int dup)
{

	long i;
	uint8_t nibble = 0;
	int16_t output;
	while(nibbles--)
	{
		uint8_t step = *buffer << nibble;
		if(nibble)
			buffer++;
		nibble^=4;
		output = nec_step(step & 0xf0, history, step_hist) << 7;
		for(i=0;i<dup;i++)
			*outbuffer++ = output;
	}
	return outbuffer;
}

// Main decoding function
void nec_decode(uint8_t *buffer, int16_t* outbuffer, long len, uint8_t div)
{
	double silence_len = NEC_SILENCE_LEN / div;

	int valid_header = 0;

	int16_t history;
	int8_t step_hist;

	long pos = 0;
	while(pos < len)
	{
		//printf("pos = %08x\n",pos);
		uint16_t nibbles;
		uint8_t byte = buffer[pos++];
		// Silence block
		if((byte & 0xc0) == 0)
		{
			if(!byte && valid_header)
				break;
			if(byte)
				valid_header = 1;

			history = 0;
			step_hist = 0;

			long buflen = silence_len * ((byte & 0x3f) + 1);
			while(buflen--)
			{
				*outbuffer++ = 0;
			}
		}
		else
		{
			int repeat_count = 1;
			uint8_t block_div = 1;

			valid_header = 1;

			// Repeat block
			if((byte & 0xc0) == 0xc0)
			{
				repeat_count = (byte & 7) + 2;
				byte = buffer[pos++];
			}
			// Read divider
			block_div = (byte & 0x3f) + 1;
			// Read # of nibbles
			if ((byte & 0xc0) == 0x80) // Skip length byte
				nibbles = buffer[pos++] + 1;
			else
				nibbles = 256;

			while(repeat_count--)
			{
				outbuffer = decode_block(buffer + pos, outbuffer, &history, &step_hist, nibbles, block_div / div);
			}
			pos += (nibbles + 1) >> 1;
		}
	}
}
