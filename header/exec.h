const char split_var = '\x02';
const char split_type = '\x01';
#include<iostream>
#include<vector>
#include<string>
#include<fstream>
#include<string>
#include<cstdlib>
using namespace std;


string join(vector <string> v, string split) {
    string res = "";
    int l = v.size();
    for (int i = 0; i < l; ++i) {
        if (i == 0) {
            res += v[i];
        }
        else {
            res += split;
            res += v[i];
        }
    }
    return res;
}
string exec(string str, string filename) {
    vector <string> func_and_param = split(str, split_var);
    vector <string> func = split(func_and_param[0], split_type);
    vector <string> headers = {"#include <fly/util.h>","#include <fly/func.h>", "#include <fly/class.h>","#include <fly/fly.h>", "using namespace std;"};

    string header = join(headers, "\n");

    string main_func = "int main(){";
    int l = func_and_param.size();
    vector <string> assigns;
    for (int i = 1; i < l; ++i) {
        vector <string> type_and_content = split(func_and_param[i], split_type);
        std::cout << type_and_content[0] << std::endl;
        if(type_and_content[0] == "int") {
            string tmp = "int ";
            tmp += i + 'a';
            tmp += " = ";
            tmp += type_and_content[1];
            tmp += ";";
            assigns.push_back(tmp);
        }
        else if (type_and_content[0] == "shared_ptr < flyvector <int> >"){
            string tmp = "shared_ptr < flyvector <int> > ";
            tmp += i + 'a';
            tmp += " = ";
            tmp += "_vector_int(\"" + type_and_content[1] + "\")";
            tmp += ";";
            assigns.push_back(tmp);
        }
        else {

        }
    }
    ofstream ofs;
    ofs.open((filename + "_in.cpp").c_str(), ios::out);
    ofs << header << endl;
    ofs << func[1] << endl;
    ofs << main_func << endl;
    for (int i = 0; i < assigns.size(); ++i)
        ofs << "\t" << assigns[i] << endl;
    //exec
    ofs << "std::cout << _string(" << func[0] << "(";
    for (int i = 1; i < l; ++i) {
        if(i == 1) {
            ofs << char(i + 'a');
        }
        else {
            ofs << "," << char(i + 'a');
        }
    }
    ofs << "));" << endl;
    ofs << "}" << endl;
    ofs.close();
    //execute
    string input_file = filename + "_in.cpp";
    string output_file = filename + "_out";
    system(("g++ -std=c++11 -pthread -o " + filename + " " + input_file).c_str());
    system(("./" + filename + " > " + output_file).c_str());
    ifstream in;
    in.open(output_file.c_str(), ios::in);
    string res;
    in >> res;
    in.close();
    return res;
}
