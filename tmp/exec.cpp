/*************************************************************************
    > File Name: exec.cpp
    > Author: Gu Shenlong
    > Mail: blackhero98@gmail.com
    > Created Time: Sat 07 May 2016 03:26:58 PM EDT
 ************************************************************************/

#include<iostream>
#include<vector>
#include<string>
#include<fstream>
#include<cstdlib>
using namespace std;
const char split_var = '\x02';
const char split_type = '\x01';
vector <string> split(string str, char split_c) {
    int l = int(str.length());
    int last = -1;
    vector <string> res;
    for (int i = 0; i <= l; ++i) {
        if (i == l || str[i] == split_c) {
            if(i - last -1 > 0) {
                res.push_back(str.substr(last+1, i - last - 1));
            }
            last = i;
        }
    }
    return res;
}
string exec(string str, string filename) {
    vector <string> func_and_param = split(str, split_var);
    vector <string> func = split(func_and_param[0], split_type);
    string header = "#include <iostream>";
    string main_func = "int main(){";
    int l = func_and_param.size();
    vector <string> assigns;
    for (int i = 1; i < l; ++i) {
        vector <string> type_and_content = split(func_and_param[i], split_type);
        if(type_and_content[0] == "int") {
            string tmp = "int ";
            tmp += i + 'a';
            tmp += " = ";
            tmp += type_and_content[1];
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
    ofs << "std::cout << " << func[0] << "(";
    for (int i = 1; i < l; ++i) {
        if(i == 1) {
            ofs << char(i + 'a');
        }
        else {
            ofs << "," << char(i + 'a');
        }
    }
    ofs << ");" << endl;
    ofs << "}" << endl;
    ofs.close();
    //execute
    string input_file = filename + "_in.cpp";
    string output_file = filename + "_out";
    system(("g++ -o " + filename + " " + input_file).c_str());
    system(("./" + filename + " > " + output_file).c_str());
    ifstream in;
    in.open(output_file.c_str(), ios::in);
    string res;
    in >> res;
    in.close();
    return res;
}
int main() {
    string test = 
        "\x02gao\x01int gao(int a,int b){return (a+b) ;}\x02int\x01";
    test += "1\x02int\x01";
    test += "2";
    string res = exec(test, "haha");
    cout << res << endl;
}
