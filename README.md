### COMS 4115 PLT Project: Fly
This is the source code for the compiler for the FLy language. <br/>
Code is translated into C++. <br/>
Features:
<ol>
<li>Basic type Inference & check</li>
<li>Lambda & Clojure</li>
<li>Go-like keyword: Fly, Chan</li>
<li>Dispatch/Exec (code and data is allowed to distribute to another machine to be executed)</li>
</ol>


# Fly

## Introduction

The Fly language is a general purpose programming language. Fly draws inspiration from Go (golang) and functional programming languages, with the aim of simplifying the development of network applications and distributed systems. The Fly compiler outputs C++ code, which is then compiled to target executable.

Fly supports similar concurrent programming features in Go such as goroutine, a light-weight thread, and channels, which are synchronized FIFO buffers for communication between light-weight threads. Fly also features asynchronous event-driven programming, type inference and extensive functional programming features such as lambda and closure. Aside from these features, Fly’s syntax and semantics resemble those of common imperative languages.

These features allow simplified implementation of various types of distributed network services and parallel computing. 

## Environment

To compile Fly, please install OCaml (https://ocaml.org/docs/install.html) and gcc version 5.3.0. Ubuntu 16.04 LTS or other Linux distrinution is recommended.

## Getting Started

Inside the Fly project folder, type make. 
    
    > make
    
This will create the fly compiler named "fly", which takes a file in fly language and outputs C++ code to both standard output and "tmp. cc" in the same project folder. 

Then type "make install" (may require sudo privilege) to copy related header files and the fly binary into search path. 

    > make install

To generate c++ code, type "fly < <fly_file_name>" (replace <fly_file_name> with your fly language file) and then tmp. cc will be generated. With tmp. cc, type "g++ -pthread -o <exec_name> -std=c++11 tmp. cc" to compile tmp. cc to the target binary file (replace <exec_name> with your binary file name).

    > fly < helloworld.fly 
    > g++ -pthread -o build_test -std=c++11 tmp.cc
    
Another way to automate the whole process is, inside the Fly project folder, type “make”, "sudo make install” and then type “make bin src=<fly_file_name> bin=<exec_name>”. This will first generate the fly compiler, and then take the fly code named <fly_file_name>, compile it down to the executable <exec_name>.

    > make 
    > sudo make install
    > make bin src=helloworld.fly bin=test_bin
    
The following sample Fly code, helloworld.fly, demonstrates the following features:

    func main () {
        print("Hello World");
        return 0;
    }

- The mandatory main function, with no parameters and return 0 as success, otherwise as failure.
- Calling the built-in print function

To compile the sample code above, type:

    > make bin src=helloworld.fly bin=test_bin

The output will be:

    > ./test_bin
    > Hello World
    
## Data Operation

### Type Inference

Fly’s declaration and assignments work as follows: A variable is declared when it is assigned a value. The value it is assigned to determines its type. Later in the scope the variable lives, it can be assigned to a different value of the same type.

    func main() {
        a = 2;
        b = "3";
        c = 4.1;
        d = true;

        print(a);
        print(b);
        print(c);
        print(d);
        return 0;
    }

Output:

    2
    3
    4.1
    1
    
### Primitive Types

Fly supports the following primitive types: Int, Float, String, Bool. Their literal expression and assignment usage are demonstrated as follows.

    3
    a = 2;
    b = "3";
    c = 4.1;
    d = true;
    
    
### Container Types

Fly provides thread-safe array and hashmap with parameterizable element types. The usage is similar to what C++ template offers. Thread-safety means the container guarantees correctness when it is accessed by multiple threads.

#### Arrays

Arrays are indexed collections of values, the value it contains must belong to the same type. Arrays can contain primitive types, class types or container types (array and hashmap). Note that in the following example, the get_at function’s behavior is undefined when the argument is greater than the max valid index.

    func main() { 
        a = @Array<#Int#>;
        a.push_back(1);
        a.push_back(2);
        a.push_back(3);
        a.push_back(4);

        print(a.size());
        print(a.get_at(3));
        return 0;
    }
    
Output:

    4
    4

#### Hashmaps

Hashmaps are collections of key and value pairs, the pairs it contains must belong to the same types of key and value. The value type in hashmap pairs can be primitive types, class types or container types (array and hashmap). The key type in hashmap pairs can only be primitive types. The behavior is undefined when the key type is anything other than primitive types.

    func main() {
        m = @Map<#Int, String#>;
        m.insert(324, "Alex");
        m.insert(38948, "Brian");
        m.insert(9238, "Chris");

        for(k: m) {
            print(k);
            print(m.get(k));
        }

        print(m.exist(324));

        m.delete(324);

        for(k: m) {
            print(k);
            print(m.get(k));
        }

        print(m.exist(324));

        return 0;
    }

Output:

    324
    Alex
    9238
    Chris
    38948
    Brian
    1
    9238
    Chris
    38948
    Brian
    0

## Control Flow

Control flow statements resemble their counterparts in C++.

### For Loop

Similar to C++ except we don’t need to specify types when declaring variables in initialization thanks to type inference.

    for (i = 1; i <= 3; i = i + 1) {
            print(i);
    }

### While Loop

    i = 1;
    while(i <= 4) {
        i = i + 1;
    }

### IF-ELSE

Fly supports both IF blocks and IF-ELSE blocks. Note that the curly brackets around the statements are mandatory. We believe this is helpful in spotting mistakes such as gotofail, Apple's SSL/TLS bug (https://gotofail.com).

    i = 1;
    if (i == 1) {
        /* Do something */
    }

    if (i == 2) {
        /* Do something */
    } else {
        /* Do something */
    }




## Function Definition

A function accept multiple arguments and returns one value. If the variable accepting the return value is declared before, it must have the same type as the return value. If the variable accepting the return value is first seen in the scope, it will have the same type as the return value, thanks to type inference.

    func main() {
        a = 1;
        a = get_int();

        return 0;
    }

    func get_int() {
        return 2;
    }

## Class

A class is user-defined type or data structure. It is declared with keyword class that its name, data and member functions. It resembles what is offered in C++ but much simpler. Everything in a class can be accessed from outside, meaning everything in a class is public. The variable declaration in a class includes the variable name and type. The member function can be defined just as global functions.

    class cat {
        name:String;
        age:Int;
        func say_sth() {
            print("I'm " + _string(age) + " years old");
        }
        func play_with(c) {
            print(name + " plays with " + c.name);
        }
    }

    func main () {
        cat1 = @cat;
        cat1.age = 12;
        cat1.name = "Mark";
    }

## Variable Passing

All the primitive types are passed-by-copy, any other types (class, containers, etc.) are passed-by-reference. 

In the following example, we first demonstrate that a Int type variable is passed-by-copy to function change_int. So after the call to change_int the value of a remains the same. Then we show that a variable “kitty” with type cat is passed into change_cat_name by reference, so after the call to change_cat_name its name changes. And then another variable named same_cat is assigned by kitty, making same_cat a reference to kitty. So a call to change_cat_name with same_cat passed in will
change the name of kitty. Lastly we show that return_same_cat returns a reference to what is passed in, making the variable still_same_cat a reference to kitty.

    func change_cat_name(c) {
        c.name = "Almighty " + c.name;
    }

    func change_int(a) {
        a = a + 1;
    }

    func return_same_cat(c) {
        return c;
    }

    func main () {
        a = 1;
        change_int(a);
        print(a);
        
        kitty = @cat;
        kitty.age = 63;
        kitty.name = "Mark";
        change_cat_name(kitty);
        print(kitty.name);

        same_cat = kitty;
        change_cat_name(same_cat);
        print(kitty.name);

        still_same_cat = return_same_cat(kitty);
        change_cat_name(still_same_cat);
        print(kitty.name);

        return 0;
    }
    
Output:

    1
    Almighty Mark
    Almighty Almighty Mark
    Almighty Almighty Almighty Mark

## Clojure

We support clojure for a function. When a functions takes less than the number of the necessary parameters, it will return a clojure of this function in which lots of parameters are bounded with the local variables. 

    func add(a, b) {
        return a + b;
    }
    a = add(1);
    b = a(3);


Finally b will be 4, and a is a clojure with the first parameter binded with the value 1.
    
## Lambda

We support a very basic of the lambda
( vars -> expr ) , takes some vars and return a expr (in the expr there maybe some local variables bounded)

    func sort_arr(a, b) {
        l = a.size();
        for(i = 1; i < l; i=i+1) {
            j = i;
            while(j > 0) {
                if(b(a.get_at(j - 1), a.get_at(j))) {
                    break;
                }
                else {
                    tmp = a.get_at(j - 1);
                    a.set_at(j - 1, a.get_at(j));
                    a.set_at(j, tmp);
                    j = j - 1;
                }
            }
        }
    }

    func main () {
        b = (x, y -> x > y);
        c = @Array<#Int#>;
        for (i = 0; i < 10; i = i + 1) {
            c.push_back(i);
        }
        sort_arr(c, b);
        print(_string(c));
        return 0;
    }


We show an sort array example using lambda, and function passing. We get a lambda b taking two arguments, return true if the first is larger then the second, otherwise false. Then we pass the lambda b to the sort function, and as a result, c will be sort in a decrease order and output
9,8,7,6,5,4,3,2,1,0.

## Dispatch /exec
    
Dispatch and exec keywords provide the ability for the function to be executed in another machine but the code is written in the similar style as the local function call.

    //Client
    func add(a, b) {
        return a + b;
    }
    a = 1;
    b = 2;
    c = add(a, b); 
    d = dispatch add(a, b, “localhost”, 5555);

    //Server

    func process(msg) {
        res = exec(msg);
        con.send(res);
    }

The first add call is a local function call, wait for the return value and c = 3, and the second dispatch call add,  the function will be executed in a server that listens to the port 5555.
For the server, the exec function will execute a string in a protocol that contains the function code and the data serialization string and get the return string to be sent back to the client.

## Fly 
In Fly language, fly is a key word meaning “spawn a thread to execute the following function”. From now on we will refer to “fly a function” as “spawn a thread to execute a function”. The semantic is similar to a normal function call except the function is run by a different, newly-created thread.

The following example flies the say_hello function 10 times, with the second argument as index. In the say_hello function, it prints out its index first and then other string and data manipulations. Because there are 10 different threads executing the function, the order of the result is non-deterministic, but each of their corresponding manipulated data must be correct.

    func main() {
        for( i = 1; i <= 10; i = i+1) {
            fly say_hello("World", i, i*2, i*3);
        }
        while(true) {

        }
        return 0;
    }

    func say_hello(str, idx, num1, num2) {
        msg = _string(idx) + " Hello " + str;
        sum = num1 + num2;
        msg = msg + " " + _string(sum);
        print(msg);
        return sum;
    }

Output (non-deterministic):

    3 Hello World 15
    1 Hello World 5
    7 Hello World 35
    5 Hello World 25
    6 Hello World 30
    4 Hello World 20
    8 Hello World 40
    2 Hello World 10
    9 Hello World 45
    10 Hello World 50


## Signal

Signal is a way for inter-thread communication. A signal is created after flying a function. When the function returns a object, the object will be sent to the function registered for receiving the signal. If no function is registered for receiving the signal, the signal member function wait() works as barrier, which blocks until the flown function returns. The wait() member function also returns the object passed from the flown function. Signal can be used to pass around different
types, including primitive types and user-defined class type. Signal can be thought of a thread-safe blocking queue which is enqueued and dequeued only once respectively.


In the following example, two calc functions are flown and s1.wait() + s2.wait() will blocks until the two calc function returns. After they both return, s1.wait() returns the value passed by the first call to calc() and s2.wait() returns the value passed by the second call to calc(). In this example the signal is used for Int type.

    func calc(a, b) {
        return a + b;
    }
    func main() {
        s1 = fly calc(1, 2);
        s2 = fly calc(3, 4);
        ans = s1.wait() + s2.wait();
        print(ans);
    } 

In the following example, both send_hello() and say_something() will be running on their own thread apart from the main thread. When send_hello() returns, say_something() will be called with the second argument being “Mark”. Note that the statement “register s say_something(“Hello”);” binds the first argument of say_something to “Hello”. Also note that there’s nothing blocking the main thread after the register statement, so a call to sleep() is placed after the register statement in case the
program exits before say_something is executed. In this example the signal is used for String type.

    func send_hello (name) {
        return name;
    }
    func say_something(greating, name) {
        print(greating + " " + name);
    }
    func main() {
        s = fly send_hello("Mark");
        register s say_something("Hello");
        sleep(1);
        return 0;
    }

Output:

    Hello Mark


## Chan

Chan is another way for inter-thread communication. The name Chan is short for Channel. It can be thought of a thread-safe blocking queue. The way Chan is declared is just as a normal class, but with the type information supplied. It can then be passed into different functions so these functions can enqueue and dequeue element inside a shared Chan even when these functions are flown on different threads. Chan can be used to pass around different types, including primitive types and
user-defined class type.

In the following example, both push and pop shares the same Chan while they run on different thread. In push(), 10 dog object are generated and then enqueued to Chan using the “<-” operator. In pop(), the “<-” operator is called to dequeue dog objects, the call to “<-” returns only when there’s already elements in the Chan, otherwise it blocks until it successfully dequeue one element from the Chan.

    class dog {
        name:String;
        age:Int;
        owner:String;
    }

    func push(c) {
        for(i = 1; i < 10; i = i + 1) {
            d = @dog;
            d.age = i;
            c <- d;
        }
    }

    func pop(c) {
        while (true) {
            d <- c;
            print("dog of age: " + _string(d.age));
        }
    }

    func main() {
        c = chan(dog);
        fly push(c);
        fly pop(c);

        sleep(1);
        return 0;
    }


Output:

    dog of age: 1
    dog of age: 2
    dog of age: 3
    dog of age: 4
    dog of age: 5
    dog of age: 6
    dog of age: 7
    dog of age: 8
    dog of age: 9

## Thread-Safe Containers

The containers Fly provides, Array and Hashmap are thread-safe. The interfaces of these containers are guaranteed to behave atomically, allowing multi-thread writing and reading. When greater granularity of synchronization is needed, the sync() member function can be called to ensure atomicity of multiple statements.

In the following example, the critical section surrounded by the curly braces is executed atomically. The scope of atomic execution is from when the sync() function is called to when the function falls out of scope (if we imagine it to be a variable).

    arr  = @Array<#Int#>;
    arr.push_back(1);
    {
        arr.sync();
        v = a.get_at(0);
        arr.set_at(0, v + 1);
    }


