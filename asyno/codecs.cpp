//
//  codecs.cpp
//  Asyno codecs
//  
//  Project asyno
//
//  Miki Fossati and Lorenzo Benoni 09/12/2016.
//
//
#include "codecs.hpp"

void vec2msg(std::vector<uchar> buff, zmq::message_t *ret)
{
    size_t bufsize = buff.capacity()*sizeof(uchar) + sizeof(buff);
    zmq::message_t temp (bufsize);
    memcpy(temp.data (), buff.data (), bufsize);
    ret->move(&temp);
}