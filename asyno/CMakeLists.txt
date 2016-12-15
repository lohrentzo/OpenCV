cmake_minimum_required(VERSION 2.8)

project( asyno )

SET (FFMPEG_DIRS
    /usr/local/include/
    )

find_package( OpenCV REQUIRED )
include_directories( ${CMAKE_CURRENT_SOURCE_DIR} ${OpenCV_INCLUDE_DIRS} ${FFMPEG_DIRS})

LINK_DIRECTORIES(/usr/local/lib)
SET (FFMPEG_LIBRARIES
  avcodec
  avformat
  avutil
  avresample
  swscale
  swresample
 )

add_executable( cvcap cvcap.cpp )
add_executable( cvproc cvproc.cpp )
add_executable( cvdisplay cvdisplay.cpp )

find_library ( ZMQ_LIB NAMES zmq )
add_library ( ASYNO_CODECS codecs.cpp )
add_library ( ASYNO_FFCODECS ffcodecs.cpp )

target_link_libraries( cvcap ${OpenCV_LIBS} ${ZMQ_LIB} ${FFMPEG_LIBRARIES} ASYNO_CODECS ASYNO_FFCODECS)
target_link_libraries( cvproc ${OpenCV_LIBS} ${ZMQ_LIB} ${FFMPEG_LIBRARIES} ASYNO_CODECS ASYNO_FFCODECS)
target_link_libraries( cvdisplay ${OpenCV_LIBS} ${ZMQ_LIB} ${FFMPEG_LIBRARIES} ASYNO_CODECS ASYNO_FFCODECS)
