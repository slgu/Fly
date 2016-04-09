#include <iostream>
#include <thread>
#include <stdio.h>      /* for printf() and fprintf() */
#include <sys/socket.h> /* for socket(), bind(), and connect() */
#include <arpa/inet.h>  /* for sockaddr_in and inet_ntoa() */
#include <stdlib.h>     /* for atoi() and exit() */
#include <string.h>     /* for memset() */
#include <unistd.h>     /* for close() */
#include <time.h>       /* for time() */
#include <netdb.h>      /* for gethostbyname() */
#include <signal.h>     /* for signal() */
#include <sys/stat.h>   /* for stat() */
#include <pthread.h>   /* for threading */

using namespace std;

//TODO implement a socket client and server

/*
 * Create a listening socket bound to the given port.
 */
static int create_server_socket(unsigned short port)
{
    int servSock;
    struct sockaddr_in servAddr;

    /* Create socket for incoming connections */
    if ((servSock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
        cout << "socket() failed" << endl;
        exit(1);
    }
      
    /* Construct local address structure */
    memset(&servAddr, 0, sizeof(servAddr));       /* Zero out structure */
    servAddr.sin_family = AF_INET;                /* Internet address family */
    servAddr.sin_addr.s_addr = htonl(INADDR_ANY); /* Any incoming interface */
    servAddr.sin_port = htons(port);              /* Local port */

    /* Bind to the local address */
    if (bind(servSock, (struct sockaddr *)&servAddr, sizeof(servAddr)) < 0) {
        cout << "bind() failed" << endl;
        exit(1);
    }

    /* Mark the socket so it will listen for incoming connections */
    if (listen(servSock, 5) < 0) {
        cout << "listen() failed" << endl;
    }

    return servSock;
}

class server {
public:
    void accept(int port);
};

void server::accept(int port) {
    int serv_sock = create_server_socket(port);
    while (1) {
        struct sockaddr_in clntAddr;
        unsigned int clntLen = sizeof(clntAddr); 
        int clntSock = ::accept(serv_sock, (struct sockaddr *)&clntAddr, &clntLen);
        if (clntSock < 0) {
            cout << "accept() failed" << endl;
            exit(1);
        }
        cout << "yes" << endl;
        // put clnt Sock to signal
    }
}

int main() {
    shared_ptr <server> ser = shared_ptr <server> (new server());
    ser->accept(5566);
    return 0;
}

