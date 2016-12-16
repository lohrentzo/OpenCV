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
#include <opencv2/opencv.hpp>

#include "codecs.hpp"
#include "ffcodecs.hpp"

int main () {
    //  Prepare our context and sockets
    zmq::context_t context (1);
    zmq::socket_t trToDis (context, ZMQ_PUSH);
    zmq::socket_t recFromDis (context, ZMQ_PULL);
    trToDis.connect ("tcp://127.0.0.1:5554");
    recFromDis.bind ("tcp://*:5552");
    zmq::socket_t trToCap (context, ZMQ_PUSH);
    zmq::socket_t recFromCap (context, ZMQ_PULL);
    trToCap.connect ("tcp://127.0.0.1:5553");
    recFromCap.bind ("tcp://*:5555");
    std::cout << "Cvproc server starting..." << std::endl;

    const int dst_width = 1280;
    const int dst_height = 720;

// Waiting for zero...
    zmq::message_t zero;
    std::cout << "cvProc: waiting for zero from cvCap..." << std::endl;
    recFromCap.recv (&zero);
    zmq::message_t answer(1);
    memcpy(answer.data(), "0", 1);
    trToCap.send(answer);
    std::cout << "cvProc: zero received!" << std::endl;
    std::cout << "cvProc: waiting for zero from cvDis..." << std::endl;
    recFromDis.recv (&zero);
    trToDis.send(answer);
    std::cout << "cvProc: ok, zero received, we're ready to go." << std::endl;
// Ok, zero received, we're ready to go.

    unsigned nb_frames = 0;
    asyno_codec_init();
    AsynoCodecContext* encoder = asyno_create_encoder_context(AVCodecID::AV_CODEC_ID_H264, dst_width, dst_height);
    AsynoCodecContext* decoder = asyno_create_decoder_context(AVCodecID::AV_CODEC_ID_H264, dst_width, dst_height);

    while (true) {
        zmq::message_t request;

        //  Wait for next request from client
        recFromCap.recv (&request);
        if (request.size() == 1)
        {
            std::string req = std::string(static_cast<char*>(request.data()), request.size());
            if (req == "0")
            {
                std::cout << "Zero received! Reset frame counter" << std::endl;
                nb_frames = 0;
                zmq::message_t answer(1);
                memcpy(answer.data(), "0", 1);
                trToDis.send(answer);
            }
        }
        else
        {
// Process the buffer
            //std::vector<uchar> req(request.size());
            //memcpy(req.data(), request.data(), request.size());
            uint8_t *req;
            size_t req_size = request.size() * sizeof(u_int8_t);
            memcpy(req, request.data(), req_size);
            cv::Mat* frame = asyno_decode_frame(req, req_size, decoder);
            //cv::Mat frame = cv::imdecode(req, cv::IMREAD_UNCHANGED);
            
/*            cv::Mat gray;
            cvtColor(*frame, gray, CV_BGR2GRAY);
            
            cv::Ptr<cv::FastFeatureDetector> fast = cv::FastFeatureDetector::create(10, false);
            std::vector<cv::KeyPoint> kp;
            
            fast->detect(gray, kp);
            
            cv::Mat rich;
            drawKeypoints(gray, kp, rich, cv::Scalar::all(-1), cv::DrawMatchesFlags::DRAW_RICH_KEYPOINTS);
*/            
            //std::vector<uchar> buff;
            //cv::imencode(".png", rich, buff);

            int len = 0;
            uint8_t* bytes = asyno_encode_frame(frame, encoder, &len);
            if (len > 0) {
                zmq::message_t reply (len);
                memcpy(reply.data (), bytes, len);
                trToDis.send (reply);
            }
            else {
                std::cout << "NO PANIC!" << std::endl;
            }
            std::cout << "Processed frame " << nb_frames << '\r' << std::flush;
            ++nb_frames;
        }
    }
    return 0;
}