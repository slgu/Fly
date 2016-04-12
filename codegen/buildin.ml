open Ast
open Sast

(* to_float, to_int, to_string, sleep, exit *)

let build_in_code =
"
float to_float(int a){
	return float(a);

float to_float(string a) {
	return stof(a);
}

int to_int(string a){
	return stoi(a);
}

string to_string(int a ){
	return to_string(a);
}
string to_string(float a ){
	return to_string(a);
}

void sleep(int seconds){
	std::chrono::seconds duration(seconds);
	std::this_thread::sleep_for(duration);
}

void exit(int exit_code){
	exit(exit_code);
}
"

let int_to_float = {
	ttkey = "";
	tfname = "to_float";
	tformals = [("int", Int)];
	tbody = [];
	tret = Float;
}

let string_to_float = {
	ttkey = "";
	tfname = "to_float";
	tformals = [("string", String)];
	tbody = [];
	tret = Float;
}

let string_to_int = {
	ttkey = "";
	tfname = "to_int";
	tformals = [("string", String)];
	tbody = [];
	tret = Int;
}

let int_to_string = {
	ttkey = "";
	tfname = "to_string";
	tformals = [("int", Int)];
	tbody = [];
	tret = String;
}

let float_to_string = {
	ttkey = "";
	tfname = "to_string";
	tformals = [("float", Float)];
	tbody = [];
	tret = String;
}

let exit_func = {
	ttkey = "";
	tfname = "exit";
	tformals = [("int", Int)];
	tbody = [];
	tret = Void;
}

let sleep_func = {
	ttkey = "";
	tfname = "sleep";
	tformals = [("int", Int)];
	tbody = [];
	tret = Void;
}

let build_in_func =
    [int_to_string;
    float_to_string;
    string_to_int;
	int_to_string;
	float_to_string;
	exit_func;
	sleep_func]

(*string to_string(void a){} *)
(*string to_string(array a){} *)
(*string to_string(set a ){} *)

(* The following defines built-in class and their member functions *)

let connection_recv = {
    ttkey = "";
    tfname = "recv";
    tformals = [];
    tbody = [];
    tret = String;
}

let connection_close = {
    ttkey = "";
    tfname = "close";
    tformals = [];
    tbody = [];
    tret = Void;
}

let connection = {
    tcname = "connection";
    member_binds = [];
    t_func_decls = [connection_recv; connection_close];
}

let server_listen = {
    ttkey = "";
    tfname = "listen";
    tformals = [("port", Int)];
    tbody = [];
    tret = Void;
}

let server_accept = {
    ttkey = "";
    tfname = "accept";
    tformals = [];
    tbody = [];
    tret = Class("connection");
}

let server = {
    tcname = "server";
    member_binds = [];
    t_func_decls = [server_listen; server_accept];
}

let build_in_class =
    [server; connection]

let build_in_class_code = ["

template <typename T> class Signal {
    public:
        condition_variable data_cond;
        mutex data_mutex;
        queue <std::shared_ptr <T>> data_queue;
        shared_ptr <T> wait() {
            std::unique_lock<std::mutex> lk(data_mutex);
            data_cond.wait(lk, [this]{return !this->data_queue.empty();});
            lk.unlock();
            auto result = data_queue.front();
            data_queue.pop();
            return result;
        }
        void notify(std::shared_ptr <T> res) {
            std::lock_guard<std::mutex> lk(data_mutex);
            data_queue.push(res);
            data_cond.notify_one();
        }
};

class connection {
private:
    int c_sock = -1;
    FILE *c_fp = NULL;
public:
    connection(int c): c_sock(c) {};
    string recv();
    void close();
};

string connection::recv() {

    string rmsg;
    char requestLine[1024] = {0};

    if (!c_fp) {
        if (c_sock < 0) {
            cout << \"connection::recv wrong socket \" << c_sock << endl;
            return rmsg;
        }
        c_fp = fdopen(c_sock, \"r\");
    }

    if (c_fp == NULL) {
        return rmsg;
    }

    fgets(requestLine, sizeof(requestLine), c_fp);

    rmsg = string(requestLine);

    return rmsg;
}

void connection::close() {
    fclose(c_fp);
    c_fp = NULL;
    c_sock = -1;
}

class server {
private:
    int create_server_socket(unsigned short port);
    int serv_sock = 0;
public:
    void listen(int port);
    shared_ptr<connection> accept(void);
};

int server::create_server_socket(unsigned short port)
{
    int servSock;
    struct sockaddr_in servAddr;

    /* Create socket for incoming connections */
    if ((servSock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
        cout << \"socket() failed\" << endl;
        exit(1);
    }

    /* Construct local address structure */
    memset(&servAddr, 0, sizeof(servAddr));       /* Zero out structure */
    servAddr.sin_family = AF_INET;                /* Internet address family */
    servAddr.sin_addr.s_addr = htonl(INADDR_ANY); /* Any incoming interface */
    servAddr.sin_port = htons(port);              /* Local port */

    /* Bind to the local address */
    if (bind(servSock, (struct sockaddr *)&servAddr, sizeof(servAddr)) < 0) {
        cout << \"bind() failed\" << endl;
        exit(1);
    }

    /* Mark the socket so it will listen for incoming connections */
    if (::listen(servSock, 5) < 0) {
        cout << \"listen() failed\" << endl;
    }

    return servSock;
}

void server::listen(int port) {
    serv_sock = create_server_socket(port);
}

shared_ptr<connection> server::accept(void) {

    struct sockaddr_in clntAddr;
    unsigned int clntLen = sizeof(clntAddr);

    int c_sock = ::accept(serv_sock, (struct sockaddr *)&clntAddr, &clntLen);

    return shared_ptr<connection>(new connection(c_sock));
}

"]
