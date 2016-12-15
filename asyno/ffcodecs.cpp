//
//  ffcodecs.cpp
//  Asyno codecs based on ffmpeg (libavcodec)
//
//  Project asyno
//
//  Miki Fossati and Lorenzo Benoni 15/12/2016.
//
//
#include <iostream>
#include <vector>

#include "ffcodecs.hpp"

AsynoCodecContext::AsynoCodecContext() : codec(NULL), codec_context(NULL), frame(NULL), swsctx(NULL), decoded_image(NULL), frame_pts(0), _type_(ASYNO_UNKNOWN) {

}

AsynoCodecContext::~AsynoCodecContext() {
  if (codec_context) avcodec_close(codec_context);
  if (frame) av_frame_free(&frame);
  if (decoded_image) delete decoded_image;
  if (swsctx) sws_freeContext(swsctx);
}

void asyno_codec_init() {
  // initialize FFmpeg library
  av_register_all();
}

void asyno_codec_uninit() {

}

AsynoCodecContext* asyno_create_encoder_context(AVCodecID codecID, int width, int height, int framerate, int bitrate) {

  AVCodec* encoder = avcodec_find_encoder(codecID);
  if (encoder == NULL) {
    return NULL;
  }
  AVCodecContext* encoderctx = avcodec_alloc_context3(encoder);
  avcodec_get_context_defaults3(encoderctx, encoder);
  encoderctx->width = width;
  encoderctx->height = height;
  encoderctx->pix_fmt = encoder->pix_fmts[0];

  const AVRational dst_fps = {framerate, 1};
  encoderctx->time_base = av_inv_q(dst_fps);
  encoderctx->framerate = dst_fps;

  encoderctx->bit_rate = bitrate;

  // open video encoder
  int ret = avcodec_open2(encoderctx, encoder, nullptr);
  if (ret < 0) {
    avcodec_close(encoderctx);
    return NULL;
  }

//  std::cout
//  << "encoder: " << encoder->name << "\n"
//  << "size:    " << width << 'x' << height << "\n"
//  << "fps:     " << av_q2d(dst_fps) << "\n"
//  << "pixfmt:  " << av_get_pix_fmt_name(encoderctx->pix_fmt) << "\n"
//  << std::flush;

  AsynoCodecContext* context = new AsynoCodecContext;
  context->codec = encoder;
  context->codec_context = encoderctx;
  context->_type_ = AsynoCodecContext::ASYNO_ENCODER;

  return context;
}

AsynoCodecContext* asyno_create_decoder_context(AVCodecID codecID, int width, int height) {
    // create new video decoder
  AVCodec* decoder = avcodec_find_decoder(codecID);
  if (decoder == NULL) {
    return NULL;
  }
  AVCodecContext* decoderctx = avcodec_alloc_context3(decoder);
  avcodec_get_context_defaults3(decoderctx, decoder);
  decoderctx->width = width;
  decoderctx->height = height;
  decoderctx->pix_fmt = AV_PIX_FMT_YUV420P;

    // open video decoder
  int ret = avcodec_open2(decoderctx, decoder, nullptr);
  if (ret < 0) {
    avcodec_close(decoderctx);
    return NULL;
  }

  // std::cout
  // << "decoder: " << decoder->name << "\n"
  // << std::flush;

  AsynoCodecContext* context = new AsynoCodecContext;
  context->codec = decoder;
  context->codec_context = decoderctx;
  context->_type_ = AsynoCodecContext::ASYNO_DECODER;

  return context;
}

uint8_t* asyno_encode_frame(cv::Mat* frame, AsynoCodecContext* context, int* len) {
  if (context == NULL) return NULL;
  if (context->codec_context == NULL) return NULL;
  if (frame == NULL) {
    if (context->frame != NULL) // created at least one pkt
      av_free_packet(&context->pkt);
    return NULL;
  }

  if (context->frame == NULL) {
      // initialize sample scaler
    context->swsctx = sws_getCachedContext(nullptr, context->codec_context->width, context->codec_context->height, AV_PIX_FMT_BGR24, context->codec_context->width, context->codec_context->height, context->codec_context->pix_fmt, SWS_BICUBIC, nullptr, nullptr, nullptr);
    if (!context->swsctx) {
      return NULL;
    }
      // allocate frame buffer for encoding
    context->frame = av_frame_alloc();
    std::vector<uint8_t> framebuf(avpicture_get_size(context->codec_context->pix_fmt, context->codec_context->width, context->codec_context->height));
    avpicture_fill(reinterpret_cast<AVPicture*>(context->frame), framebuf.data(), context->codec_context->pix_fmt, context->codec_context->width, context->codec_context->height);
    context->frame->width = context->codec_context->width;
    context->frame->height = context->codec_context->height;
    context->frame->format = static_cast<int>(context->codec_context->pix_fmt);
  }
  else {
    av_free_packet(&context->pkt);
  }

  int got_pkt = 0;
  if (frame == NULL) {
    avcodec_encode_video2(context->codec_context, &context->pkt, nullptr, &got_pkt);
    return NULL;
  }

    // convert cv::Mat(OpenCV) to AVFrame(FFmpeg)
  const int stride[] = { static_cast<int>(frame->step[0]) };
  sws_scale(context->swsctx, &frame->data, stride, 0, frame->rows, context->frame->data, context->frame->linesize);
  context->frame->pts = context->frame_pts++;

    // encode video frame
  context->pkt.data = nullptr;
  context->pkt.size = 0;
  av_init_packet(&context->pkt);

  uint8_t* retbuf = NULL;
  if (len) *len = 0;
  int ret = avcodec_encode_video2(context->codec_context, &context->pkt, context->frame, &got_pkt);
  if (ret < 0) {
  }
  if (got_pkt) {
    retbuf = context->pkt.data;
    if (len) *len = context->pkt.size;
  }
  return retbuf;
}

cv::Mat* asyno_decode_frame(uint8_t* bytes, int len, AsynoCodecContext* context) {
  int got_frame = 0;

  if (bytes == NULL) {
   avcodec_decode_video2(context->codec_context, context->frame, &got_frame, NULL);
   return NULL;
  }

  if (len <= 0) return NULL;
  if (context == NULL) return NULL;
  if (context->codec_context == NULL) return NULL;

  if (context->frame == NULL) {
    context->frame = av_frame_alloc();
  }

  std::vector<uint8_t> framebuf(avpicture_get_size(context->codec_context->pix_fmt, context->codec_context->width, context->codec_context->height));
  avpicture_fill(reinterpret_cast<AVPicture*>(context->frame), framebuf.data(), context->codec_context->pix_fmt, context->codec_context->width, context->codec_context->height);
  context->frame->width = context->codec_context->width;
  context->frame->height = context->codec_context->height;
  context->frame->format = static_cast<int>(context->codec_context->pix_fmt);

  AVPacket pkt;
  pkt.data = bytes;
  pkt.size = len;
  av_init_packet(&pkt);

  int ret = avcodec_decode_video2(context->codec_context, context->frame, &got_frame, &pkt);
  if (ret < 0) return NULL;
  if (got_frame <= 0) return NULL;

  int w = context->codec_context->width;
  int h = context->codec_context->height;

  AVFrame dst;
  cv::Mat* decoded_image = new cv::Mat(h, w, CV_8UC3);
  dst.data[0] = (uint8_t *)decoded_image->data;

  avpicture_fill( (AVPicture *)&dst, dst.data[0], AV_PIX_FMT_BGR24, w, h);
  SwsContext *convert_ctx = sws_getContext(w, h, context->codec_context->pix_fmt, w, h, AV_PIX_FMT_BGR24, SWS_FAST_BILINEAR, NULL, NULL, NULL);
  sws_scale(convert_ctx, context->frame->data, context->frame->linesize, 0, h, dst.data, dst.linesize);
  return decoded_image;
}

void asyno_release_context(AsynoCodecContext* context) {
  if (context->_type_ == AsynoCodecContext::ASYNO_ENCODER) {
    asyno_encode_frame(NULL, context, NULL);
  }
  else if (context->_type_ == AsynoCodecContext::ASYNO_ENCODER) {
    asyno_decode_frame(NULL, 0, context);
  }
  delete context;
}



/*
int main(int argc, char* argv[])
{
  asyno_codec_init();
    // av_log_set_level(AV_LOG_DEBUG);

  const int dst_width = 1280;
  const int dst_height = 720;

    // initialize OpenCV capture as input frame generator
  cv::VideoCapture cvcap(0);
  if (!cvcap.isOpened()) {
    std::cerr << "fail to open cv::VideoCapture";
    return 2;
  }
  cvcap.set(CV_CAP_PROP_FRAME_WIDTH, dst_width);
  cvcap.set(CV_CAP_PROP_FRAME_HEIGHT, dst_height);

    // allocate cv::Mat with extra bytes (required by AVFrame::data)
  std::vector<uint8_t> imgbuf(dst_height * dst_width * 3 + 16);
  cv::Mat image(dst_height, dst_width, CV_8UC3, imgbuf.data(), dst_width * 3);

  AsynoCodecContext* encoder = asyno_create_encoder_context(AVCodecID::AV_CODEC_ID_H264, dst_width, dst_height);
  AsynoCodecContext* decoder = asyno_create_decoder_context(AVCodecID::AV_CODEC_ID_H264, dst_width, dst_height);

  bool end_of_stream = false;
  do {
    if (!end_of_stream) {
        // retrieve source image
      cvcap >> image;

      int len = 0;
      uint8_t* bytes = asyno_encode_frame(&image, encoder, &len);

      if (bytes != NULL && len > 0) {
        cv::Mat* decoded_image = asyno_decode_frame(bytes, len, decoder);
        if (decoded_image) {
          cv::imshow("press ESC to exit", *decoded_image);
          delete decoded_image;
          if (cv::waitKey(33) == 0x1b)
            end_of_stream = true;
        }
      }
    }
  } while (!end_of_stream);
  return 0;
}
*/
