// This autogenerated skeleton file illustrates how to build a server.
// You should copy it to another filename to avoid overwriting it.

#include "OpenCV/Core/MatUtil.h"
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/server/TSimpleServer.h>
#include <thrift/transport/TServerSocket.h>
#include <thrift/transport/TBufferTransports.h>

using namespace ::apache::thrift;
using namespace ::apache::thrift::protocol;
using namespace ::apache::thrift::transport;
using namespace ::apache::thrift::server;

using boost::shared_ptr;

class MatUtilHandler : virtual public MatUtilIf {
 public:
  MatUtilHandler() {
    // Your initialization goes here
  }

  void pack( ::Mat& _return, const  ::CVType::type type, const  ::MatUnpacked& matUnpacked) {
    // Your implementation goes here
    printf("pack\n");
  }

  void unpack( ::MatUnpacked& _return, const  ::Mat& mat) {
    // Your implementation goes here
    printf("unpack\n");
  }

};

int main(int argc, char **argv) {
  int port = 9090;
  shared_ptr<MatUtilHandler> handler(new MatUtilHandler());
  shared_ptr<TProcessor> processor(new MatUtilProcessor(handler));
  shared_ptr<TServerTransport> serverTransport(new TServerSocket(port));
  shared_ptr<TTransportFactory> transportFactory(new TBufferedTransportFactory());
  shared_ptr<TProtocolFactory> protocolFactory(new TBinaryProtocolFactory());

  TSimpleServer server(processor, serverTransport, transportFactory, protocolFactory);
  server.serve();
  return 0;
}

