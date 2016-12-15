//
//  ffcodecs.hpp
//  Asyno codecs based on ffmpeg (libavcodec)
//
//  Project asyno
//
//  Miki Fossati and Lorenzo Benoni 15/12/2016.
//
//
#ifndef FF_ASYNO_CODECS
#define FF_ASYNO_CODECS

// FFmpeg
extern "C" {
#include <libavformat/avformat.h>
#include <libavcodec/avcodec.h>
#include <libavutil/avutil.h>
#include <libavutil/pixdesc.h>
#include <libswscale/swscale.h>
}

// OpenCV
#include <opencv2/opencv.hpp>
#include <opencv2/highgui.hpp>

struct AsynoCodecContext {

  enum AsynoContextType {
    ASYNO_UNKNOWN,
    ASYNO_ENCODER,
    ASYNO_DECODER
  };

  AsynoCodecContext();
  ~AsynoCodecContext();

  AVCodec* codec;
  AVCodecContext* codec_context;
  AVFrame* frame;
  SwsContext* swsctx;
  cv::Mat* decoded_image;
  int64_t frame_pts;
  AVPacket pkt;

  AsynoContextType _type_;
};


void asyno_codec_init();
void asyno_codec_uninit();
AsynoCodecContext* asyno_create_encoder_context(AVCodecID codecID, int width, int height, int framerate = 30, int bitrate = 512000);
AsynoCodecContext* asyno_create_decoder_context(AVCodecID codecID, int width, int height);
uint8_t* asyno_encode_frame(cv::Mat* frame, AsynoCodecContext* context, int* len);
cv::Mat* asyno_decode_frame(uint8_t* bytes, int len, AsynoCodecContext* context);
void asyno_release_context(AsynoCodecContext* context);

#endif
