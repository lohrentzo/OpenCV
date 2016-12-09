//
//  cvcap.cpp
//  Frame Capture
//  
//  Project asyno
//
//  Miki Fossati and Lorenzo Benoni 09/12/2016.
//
//
#include "zmq.hpp"
#include <string>
#include <iostream>
#include "opencv2/opencv.hpp"

#include "codecs.hpp"

cv::String window_name = "Ciaone proprio";

int main( void )
{
    zmq::context_t context (1);
    zmq::socket_t transmitter (context, ZMQ_PUSH);
    zmq::socket_t receiver (context, ZMQ_PULL);
    cv::VideoCapture cap;
    cv::Mat frame;

    std::cout << "Connecting to server..." << std::endl;
    transmitter.connect ("tcp://127.0.0.1:5555");
    receiver.bind ("tcp://*:5553");

    cap.open( 0 );
    if(!cap.isOpened()) { printf("--(!)Error opening video capture\n"); return -1; }

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

    while (  cap.read(frame) )
    {
        if( frame.empty() )
        {
            printf(" --(!) No captured frame -- Break!");
            break;
        }

        std::vector<uchar> buff;
        cv::imencode(".png", frame, buff);
        zmq::message_t request;
        vec2msg(buff, &request); 
        transmitter.send (request);
        zmq::message_t reply;
        receiver.recv (&reply);
        std::vector<uchar> rep(reply.size());
        memcpy(rep.data(), reply.data(), reply.size());
        cv::Mat newframe = cv::imdecode(rep, cv::IMREAD_UNCHANGED);
        std::cout << nb_frames << '\r' << std::flush;
        ++nb_frames;

        cv::imshow( window_name, newframe );

        int c = cv::waitKey(10);
        if( (char)c == 27 ) { break; } // escape
    }
    return 0;
}