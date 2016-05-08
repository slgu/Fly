string _string(shared_ptr <flyvector <int> > v) {
    int l = v->size();
    string res = "";
    for (int i = 0; i < l; ++i) {
        if (i == 0) {
            res += _string(v->get_at(i));
        }
        else {
            res += ",";
            res += _string(v->get_at(i));
        }
    }
    return res;
}
