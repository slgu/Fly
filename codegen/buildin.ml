open Ast
open Sast

(* to_float, to_int, to_string, sleep, exit *)

let build_in_code =
[
"
int len(string a) {
    return a.length();
}

void print(bool a) {
    std::stringstream stream;
    stream << a << endl;
	cout << stream.str();
}
void print(int a) {
    std::stringstream stream;
    stream << a << endl;
	cout << stream.str();
}
void print(size_t a) {
    print((int)a);
}
void print(string a) {
    std::stringstream stream;
    stream << a << endl;
	cout << stream.str();
}
void print(float a) {
    std::stringstream stream;
    stream << a << endl;
	cout << stream.str();
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

let len = {
	ttkey = "";
	tfname = "len";
	tformals = [("a", String)];
	tbody = [];
	tret = Int;
}

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

let print_bool = {
	ttkey = "";
	tfname = "print";
	tformals = [("bool", Bool)];
	tbody = [];
	tret = Void;
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
	print_float;
	print_bool;
    len]



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

let connection_is_alive = {
    ttkey = "";
    tfname = "is_alive";
    tformals = [];
    tbody = [];
    tret = Bool;
}

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
    t_func_decls = [connection_recv; connection_close; connection_send; connection_is_alive];
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

let client_connect = {
    ttkey = "";
    tfname = "connect";
    tformals = [("server_ip", String);("port", Int)];
    tbody = [];
    tret = Class("connection");
}

let client = {
    tcname = "client";
    member_binds = [];
    t_func_decls = [client_connect];
}

let build_in_class =
    [server; connection; client]

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


(*get the return type of array, fail if not ok*)
let get_arr_call_ret (thistype:typ) fname expr_types = match thistype with
    | Array x ->
        let expr_len = List.length expr_types
        in
        begin match fname with
        | "push_back" ->
            if expr_len = 1 then
                if [x] = expr_types then Void
                else failwith ("type not consistent: get_arr_call_ret")
            else
                failwith ("push_back not 1 element: get_arr_call_ret")
        | "get_at" ->
            if expr_len = 1 then
                if [Int] = expr_types then x
                else failwith ("type not consistent: get_arr_call_ret")
            else
                failwith ("get_at not 1 element: get_arr_call_ret")
        | "size" ->
            if expr_len = 0 then
                Int
            else
                failwith("size should 0 element: get_arr_call_ret")
        | _ ->
            failwith ("not support build in array function")
        end
    | _ -> failwith ("not array error")

(*get the return type of map, fail if not ok*)
let get_map_call_ret (thistype:typ) fname expr_types = match thistype with
    | Map (x,y) ->
        let expr_len = List.length expr_types
        in
        begin match fname with
        | "insert" ->
            if expr_len = 2 then
                if [x;y] = expr_types then Void
                else failwith ("type not consistent: get_map_call_ret")
            else
                failwith ("insert not 2 element: get_map_call_ret")
        | "get" ->
            if expr_len = 1 then
                if [x] = expr_types then y
                else failwith ("type not consistent: get_map_call_ret")
            else
                failwith ("get_at not 1 element: get_map_call_ret")
        | "size" ->
            if expr_len = 0 then
                Int
            else
                failwith("size should 0 element: get_map_call_ret")
        | "delete" ->
            if expr_len = 1 then
                if [x] = expr_types then Void
                else failwith ("type not consistent: get_map_call_ret")
            else
                failwith("delete should 1 element: get_map_call_ret")
        | "exist" ->
            if expr_len = 1 then
                if [x] = expr_types then Bool
                else failwith  ("type not consistent: get_map_call_ret")
            else
                failwith("exist should be 1 element: get_map_call_ret")
        | _ ->
            failwith ("not support build in map function")
        end
    | _ -> failwith ("not array error")

let build_in_class_code = ["

template <typename T> class Signal {
    public:
        condition_variable data_cond;
        mutex data_mutex;
        queue <std::shared_ptr <T>> data_queue;
        shared_ptr <T> wait() {
            std::unique_lock<std::mutex> lk(data_mutex);
            data_cond.wait(lk, [this]{return !this->data_queue.empty();});
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

template <typename T> class Chan {
private:
    mutable std::mutex mut;
    std::queue <std::shared_ptr <T>> data_queue;
    std::condition_variable data_cond;
public:
    Chan(){

    }
    std::shared_ptr <T> wait_and_pop() {
        std::unique_lock <std::mutex> lk(mut);
        data_cond.wait(lk, [this]{return !data_queue.empty();});
        std::shared_ptr <T> res = data_queue.front();
        data_queue.pop();
        return res;
    }
    void push(std::shared_ptr <T> tmp) {
        std::lock_guard <std::mutex> lk(mut);
        data_queue.push(tmp);
        data_cond.notify_one();
    }
};

template<class K, class V>
class flymap
{
    std::map<K,V> m;
public:
    std::recursive_mutex m_mutex;

    int size() {
        std::unique_lock<std::recursive_mutex> lk(m_mutex);
        return m.size();
    }

    V& operator[] (const K& k) {
        std::unique_lock<std::recursive_mutex> lk(m_mutex);
        return m[k];
    }

    void erase (const K& k) {
        std::unique_lock<std::recursive_mutex> lk(m_mutex);
        m.erase(k);
    }

    typename std::map<K,V>::iterator find (const K& k) {
        std::unique_lock<std::recursive_mutex> lk(m_mutex);
        return m.find(k);
    }

    typename std::map<K,V>::iterator end (void) {
        return m.end();
    }

    typename std::map<K,V>::iterator begin (void) {
        return m.begin();
    }

    void insert (const K& k, const V& v) {
        std::unique_lock<std::recursive_mutex> lk(m_mutex);
        m[k] = v;
    }
};

class connection {
private:
    int c_sock = -1;
    FILE *c_fp = NULL;
    bool alive;
public:
    connection(int c, bool al): c_sock(c), alive(al) {};
    string recv();
    bool send(string s);
    void close();
    bool is_alive();
};

bool connection::is_alive() {
    return alive;
}

bool connection::send(string msg) {
    if (c_sock < 0) {
        cout << \"connection::send wrong socket \" << c_sock << endl;
        alive = false;
        return false;
    }

    msg += \"\\n\";

    int len = msg.length();
    if (::send(c_sock, msg.c_str(), len, 0) != len) {
        cout << \"connection::send fail \" << endl;
        alive = false;
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
        alive = false;
        return rmsg;
    }

    if (!fgets(requestLine, sizeof(requestLine), c_fp)) {
        alive = false;
        return rmsg;
    }

    rmsg = string(requestLine);
    rmsg.pop_back();

    return rmsg;
}

void connection::close() {
    fclose(c_fp);
    c_fp = NULL;
    c_sock = -1;
    alive = false;
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

    return shared_ptr<connection>(new connection(c_sock, true));
}

class client {
public:
    shared_ptr<connection> connect(string server_ip, int port) {

        int sockfd;
        struct sockaddr_in serv_addr;

        shared_ptr<connection> fail(new connection(sockfd, false));

        if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
        {
            printf(\"Error : Could not create socket \");
            return fail;
        }

        memset(&serv_addr, '0', sizeof(serv_addr));
        serv_addr.sin_family = AF_INET;
        serv_addr.sin_port = htons((short)port);

        if (inet_pton(AF_INET, server_ip.c_str(), &serv_addr.sin_addr)<=0)
        {
            printf(\" inet_pton error occured\");
            return fail;
        }

        if ( ::connect(sockfd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0)
        {
            printf(\" Error : Connect Failed \");
            return fail;
        }

        return shared_ptr<connection> (new connection(sockfd, true));
    }
};


"]
