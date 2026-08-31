/*
	Encode and decode algorithms for
	NEC uPD775x ADPCM

	2022 by superctr.
*/

#include <stdint.h>

/**
 * Encode uPD7759 ADPCM samples
 *
 * \param buffer
 *        Input samples in 16-bit PCM format
 * \param outbuffer
 *        Output samples. The size of outbuffer should be at least (len) bytes
 * \param len
 *        The number of samples to encode
 * \param div
 *        Clock divider
 *        For uPD7759, this should be a value between 9 and 32, the sample rate
 *        in Hz will be (chip clock / 4 / rate)
 * \param threshold
 *        Threshold for silence detection.
 * \param master
 *        Set to true to enable RLE compression. This is only supported with the
 *        uPD7759 running in a "master" configuration (reading from external ROM)
 */
uint8_t* nec_encode(int16_t* buffer, uint8_t* outbuffer, long len, uint8_t div, int16_t threshold, int master);

/**
 * Get the clock divider of an uPD7759 sample.
 *
 * Since uPD7759 can (theoretically) change the sample rate in the middle of a sample,
 * a pass through the sample data is necessary to check if the sample rate is consistent.
 *
 * If this function returns 0, the sample is most probably not valid and further decoding
 * should be aborted.
 */
uint8_t nec_get_divider(uint8_t *buffer, long len);

/**
 * Get the length of the output buffer necessary to decode an uPD7759 sample
 *
 * Call this after nec_get_divider
 */
long nec_get_length(uint8_t *buffer, long len, uint8_t div);

/**
 * Decode uPD7759 sample
 */
void nec_decode(uint8_t *buffer, int16_t* outbuffer, long len, uint8_t div);

//=====================================================================

/**
 * Given (len) amount of PCM samples in buffer,
 * return encoded *raw* ADPCM samples in outbuffer.
 * Output buffer should be at least (len/2) elements large.
 *
 * Note that since uPD7759 samples are usually encapsulated,
 * this function is normally not used
 */
void nec_encode_raw(int16_t *buffer,uint8_t *outbuffer,long len);

/**
 * Given *raw* ADPCM samples in (buffer), return (len) amount of
 * decoded PCM samples in (outbuffer).
 * Output buffer should be at least (len*2) elements large.
 *
 * Note that since uPD7759 samples are usually encapsulated,
 * this function is normally not used
 */
void nec_decode_raw(uint8_t *buffer,int16_t *outbuffer,long len);
