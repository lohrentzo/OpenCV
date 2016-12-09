//
//  cvproc.cpp
//  Frame processor
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

int main () {
    //  Prepare our context and sockets
    zmq::context_t context (1);
    zmq::socket_t transmitter (context, ZMQ_PUSH);
    zmq::socket_t receiver (context, ZMQ_PULL);
    transmitter.connect ("tcp://127.0.0.1:5554");
    receiver.bind ("tcp://*:5555");
    std::cout << "Cvproc server starting..." << std::endl;

// Waiting for zero...
    zmq::message_t zero;
    std::cout << "Waiting for zero..." << std::endl;
    receiver.recv (&zero);
    zmq::message_t answer(1);
    memcpy(answer.data(), "0", 1);
    transmitter.send(answer);
    std::cout << "Zero received! Ready to go..." << std::endl;
// Ok, zero received, we're ready to go.

    unsigned nb_frames = 0;

    while (true) {
        zmq::message_t request;

        //  Wait for next request from client
        receiver.recv (&request);
        if (request.size() == 1)
        {
            std::string req = std::string(static_cast<char*>(request.data()), request.size());
            if (req == "0")
            {
                std::cout << "Zero received! Reset frame counter" << std::endl;
                nb_frames = 0;
                zmq::message_t answer(1);
                memcpy(answer.data(), "0", 1);
                transmitter.send(answer);
            }
        }
        else
        {
// Process the buffer
            std::vector<uchar> req(request.size());
            memcpy(req.data(), request.data(), request.size());
            cv::Mat frame = cv::imdecode(req, cv::IMREAD_UNCHANGED);
            
            cv::Mat gray;
            cvtColor(frame, gray, CV_BGR2GRAY);
            
            cv::Ptr<cv::FastFeatureDetector> fast = cv::FastFeatureDetector::create(10, false);
            std::vector<cv::KeyPoint> kp;
            
            fast->detect(gray, kp);
            
            cv::Mat rich;
            drawKeypoints(gray, kp, rich, cv::Scalar::all(-1), cv::DrawMatchesFlags::DRAW_RICH_KEYPOINTS);
            
            std::vector<uchar> buff;
            cv::imencode(".png", rich, buff);

            zmq::message_t reply;
            vec2msg(buff, &reply);
            transmitter.send (reply);
            std::cout << "Processed frame " << nb_frames << '\r' << std::flush;
            ++nb_frames;
        }
    }
    return 0;
}