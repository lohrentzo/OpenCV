cmake_minimum_required(VERSION 3.10)

project( asyno )

set( CMAKE_CXX_STANDARD 17 )
set( CMAKE_CXX_STANDARD_REQUIRED ON )
set( FFMPEG_DIRS
    /usr/local/include/
    )
set (FFMPEG_LIBRARIES
  avcodec
  avformat
  avutil
  avresample
  swscale
  swresample
 )

find_package( OpenCV REQUIRED )
include_directories( ${CMAKE_CURRENT_SOURCE_DIR} ${OpenCV_INCLUDE_DIRS} ${FFMPEG_DIRS} )

link_directories( /usr/local/lib )

add_executable( cvcap cvcap.cpp )
add_executable( cvproc cvproc.cpp )
add_executable( cvdisplay cvdisplay.cpp )

find_library ( ZMQ_LIB NAMES zmq )
add_library ( ASYNO_CODECS codecs.cpp )
add_library ( ASYNO_FFCODECS ffcodecs.cpp )

target_link_libraries( cvcap ${OpenCV_LIBS} ${ZMQ_LIB} ${FFMPEG_LIBRARIES} ASYNO_CODECS ASYNO_FFCODECS )
target_link_libraries( cvproc ${OpenCV_LIBS} ${ZMQ_LIB} ${FFMPEG_LIBRARIES} ASYNO_CODECS ASYNO_FFCODECS )
target_link_libraries( cvdisplay ${OpenCV_LIBS} ${ZMQ_LIB} ${FFMPEG_LIBRARIES} ASYNO_CODECS ASYNO_FFCODECS )
