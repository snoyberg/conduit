#include <zlib.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

z_stream * create_z_stream (void)
{
	z_stream *ret = malloc(sizeof(z_stream));
	ret->zalloc = Z_NULL;
	ret->zfree = Z_NULL;
	ret->opaque = Z_NULL;
	ret->next_in = NULL;
	ret->avail_in = 0;

	if (deflateInit2(ret, 7, Z_DEFLATED, 31, 8, Z_DEFAULT_STRATEGY)
		!= Z_OK)                 return NULL;
	else                             return ret;
}

void free_z_stream (z_stream *stream)
{
	deflateEnd(stream);
	free(stream);
}

void set_avail_in (z_stream *stream, char *buff, unsigned int avail)
{
	stream->next_in = buff;
	stream->avail_in = avail;
}

void set_avail_out (z_stream *stream, char *buff, unsigned int avail)
{
	stream->next_out = buff;
	stream->avail_out = avail;
}

void call_deflate_noflush (z_stream *stream)
{
	int ret;
	ret = deflate(stream, Z_NO_FLUSH);
	assert (ret == Z_OK);
}

int call_deflate_finish (z_stream *stream)
{
	int ret;
	ret = deflate(stream, Z_FINISH);
	assert (ret == Z_OK || ret == Z_STREAM_END);
	return ret;
}

unsigned int get_avail_out (z_stream *stream)
{
	return stream->avail_out;
}
