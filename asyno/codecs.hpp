//
//  codecs.hpp
//  Asyno codecs
//  
//  Project asyno
//
//  Miki Fossati and Lorenzo Benoni 09/12/2016.
//
//
#ifndef ASYNO_CODECS
#define ASYNO_CODECS

#include "zmq.hpp"
#include "opencv2/opencv.hpp"

void vec2msg(std::vector<uchar> buf, zmq::message_t *ret);

#endif /* ASYNO_CODECS */