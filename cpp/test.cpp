#include <cstdio>
#include <memory>
void take();
class A;
class B;
class A {
public:
    std::shared_ptr <B> guy;
    void play();
    void play2();
};

class B {
public:
    int b;
    std::shared_ptr <A> guy;
    void play();
};

void A::play() {
    take();
    guy->play();
}

void A::play2() {
    printf("123123\n");
}

void B::play() {
    printf("%d\n", b);
    printf("asdad\n");
}

void take() {
    A a;
    a.play2();
}

int main() {
    take();
    A a;
    a.guy.reset(new B());
    a.guy->b = 1;
    a.play();
    std::shared_ptr <A> b = std::shared_ptr <A>(new A());
}
