open Ast
open Sast

(* to_float, to_int, to_string, sleep, exit *)

let build_in_code =
[
"
void print(int a) {
	cout << a << endl;
}
void print(string a) {
	cout << a << endl;
}
void print(float a) {
	cout << a << endl;
}
float _float(int a){
	return float(a);
}
float _float(string a) {
	return stof(a);
}

int _int(string a){
	return stoi(a);
}

string _string(int a ){
	return to_string(a);
}
string _string(float a ){
	return to_string(a);
}

void _sleep(int seconds){
	std::chrono::seconds duration(seconds);
	std::this_thread::sleep_for(duration);
}

void _exit(int exit_code){
	exit(exit_code);
}
"
]

let int_to_float = {
	ttkey = "";
	tfname = "_float";
	tformals = [("int", Int)];
	tbody = [];
	tret = Float;
}

let string_to_float = {
	ttkey = "";
	tfname = "_float";
	tformals = [("string", String)];
	tbody = [];
	tret = Float;
}

let print_float = {
	ttkey = "";
	tfname = "print";
	tformals = [("float", Float)];
	tbody = [];
	tret = Void;
}

let print_int = {
	ttkey = "";
	tfname = "print";
	tformals = [("int", Int)];
	tbody = [];
	tret = Void;
}

let print_str = {
	ttkey = "";
	tfname = "print";
	tformals = [("string", String)];
	tbody = [];
	tret = Void;
}

let string_to_int = {
	ttkey = "";
	tfname = "_int";
	tformals = [("string", String)];
	tbody = [];
	tret = Int;
}

let int_to_string = {
	ttkey = "";
	tfname = "_string";
	tformals = [("int", Int)];
	tbody = [];
	tret = String;
}

let float_to_string = {
	ttkey = "";
	tfname = "_string";
	tformals = [("float", Float)];
	tbody = [];
	tret = String;
}

let exit_func = {
	ttkey = "";
	tfname = "_exit";
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
	string_to_float;
	int_to_float;
	exit_func;
	sleep_func;
	print_str;
	print_int;
	print_float]



let rec match_build_in fname type_list =
	let rec inner_func funcs fname type_list = match funcs with
	| [] -> None
	| (x::y) -> begin match x with
		| {tfname=thisfname;tformals=binds;_}->
			let thistype_list = List.map snd binds
			in if type_list = thistype_list && fname = thisfname
			then Some x
			else inner_func y fname type_list
		end
	in
	inner_func build_in_func fname type_list

let rec check_build_in_name fname =
	List.exists (fun item -> match item with
	| {tfname=thisfname;_} -> if fname = thisfname then true else false) build_in_func

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

let connection_send = {
    ttkey = "";
    tfname = "send";
    tformals = [("msg", String)];
    tbody = [];
    tret = Bool;
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
    t_func_decls = [connection_recv; connection_close; connection_send];
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

let check_build_in_class cname =
	List.exists (fun item -> match item with
		| {tcname=thiscname;_}-> if cname = thiscname then true else false) build_in_class

let match_build_in_objcall cname fname type_list =
	let match_func tfdecl fname type_list = match tfdecl with
		| {tfname=thisfname;tformals=binds;_} ->
			let thistype_list = List.map snd binds
			in if thisfname = fname && thistype_list = type_list
			then true
			else false
	in
	let rec match_funcs tfdecls fname type_list = match tfdecls with
		| [] -> None
		| (x::y) -> if match_func x fname type_list then Some x else match_funcs y fname type_list
	in
	let rec inner_func classes cname fname type_list = match classes with
		| [] -> None
		| (x::y) ->
			begin
			match x with
			| {tcname=thiscname;t_func_decls=tfdecls;_} ->
				if thiscname = cname
				then match_funcs tfdecls fname type_list
				else inner_func y cname fname type_list
			end
	in
	inner_func build_in_class cname fname type_list

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
    bool send(string s);
    void close();
};

bool connection::send(string msg) {
    if (c_sock < 0) {
        cout << \"connection::send wrong socket \" << c_sock << endl;
        return false;
    }

    msg += \"\\n\";

    int len = msg.length();
    if (::send(c_sock, msg.c_str(), len, 0) != len) {
        cout << \"connection::send fail \" << endl;
        return false;
    }

    return true;
}

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
    signal(SIGPIPE, SIG_IGN);
    serv_sock = create_server_socket(port);
}

shared_ptr<connection> server::accept(void) {

    struct sockaddr_in clntAddr;
    unsigned int clntLen = sizeof(clntAddr);

    int c_sock = ::accept(serv_sock, (struct sockaddr *)&clntAddr, &clntLen);

    return shared_ptr<connection>(new connection(c_sock));
}

"]
