//
//  cvdisplay.cpp
//  
//  Project asyno
//
//  Miki Fossati and Lorenzo Benoni 09/12/2016.
//
//
#include "zmq.hpp"
#include <iostream>
#include <opencv2/opencv.hpp>

#include "ffcodecs.hpp"

cv::String window_name = "Ciaone proprio";

int main( void )
{
    zmq::context_t context (1);
    zmq::socket_t transmitter (context, ZMQ_PUSH);
    zmq::socket_t receiver (context, ZMQ_PULL);
    transmitter.connect ("tcp://127.0.0.1:5552");
    receiver.bind ("tcp://*:5554");
    std::cout << "Cvproc server starting..." << std::endl;

    const int dst_width = 1280;
    const int dst_height = 720;

// Sending 0 and waiting for answer
    zmq::message_t zero(1);
    memcpy(zero.data(), "0", 1);
    std::cout << "Sending zero..." << std::endl;
    transmitter.send(zero);
    zmq::message_t answer;
    receiver.recv (&answer);
    std::cout << "Answer received! Ready to go..." << std::endl;
// Ok, answer received, we're good to go

    unsigned nb_frames = 0;
    asyno_codec_init();
    AsynoCodecContext* decoder = asyno_create_decoder_context(AVCodecID::AV_CODEC_ID_H264, dst_width, dst_height);

    while (true) {
        zmq::message_t buffer;
        receiver.recv (&buffer);
        uint8_t *req = reinterpret_cast<uint8_t*>(buffer.data());
        memcpy(req, buffer.data(), buffer.size());
        cv::Mat* frame = asyno_decode_frame(req, buffer.size(), decoder);
        
        //std::vector<uchar> buf(buffer.size());
        //memcpy(buf.data(), buffer.data(), buffer.size());
        //cv::Mat frame = cv::imdecode(buf, cv::IMREAD_UNCHANGED);

        cv::imshow( window_name, *frame );

        int c = cv::waitKey(10);
        if( (char)c == 27 ) { break; } // escape

        std::cout << nb_frames << '\r' << std::flush;
        ++nb_frames;
    }
}