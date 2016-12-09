cmake_minimum_required(VERSION 2.8)

project( asyno )

find_package( OpenCV REQUIRED )
include_directories( ${CMAKE_CURRENT_SOURCE_DIR} ${OpenCV_INCLUDE_DIRS} )

add_executable( cvcap cvcap.cpp )
add_executable( cvproc cvproc.cpp )
add_executable( cvboh cvboh.cpp )

find_library ( ZMQ_LIB NAMES zmq )
add_library ( ASYNO_CODECS codecs.cpp )

target_link_libraries( cvcap ${OpenCV_LIBS} ${ZMQ_LIB} ASYNO_CODECS )
target_link_libraries( cvproc ${OpenCV_LIBS} ${ZMQ_LIB} ASYNO_CODECS )
target_link_libraries( cvboh ${OpenCV_LIBS} ${ZMQ_LIB} ASYNO_CODECS )