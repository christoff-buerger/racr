// This program and the accompanying materials are made available under the
// terms of the MIT license (X11 license) which accompanies this distribution.

// Author: C. Bürger

class C
{
public:
    static int a;
    class D;
};

class C::D
{
public:
    static int a;
	class E;
    class F;
    static void m(int h)
    {
        //E::a = h; // E only declared but not defined yet.
        //F::a = h; // F only declared but not defined yet.
    }
};

class C::D::E
{
public:
    static int a;
    static void m(int h)
    {
        D::a = h;
        F::a = h; // F only declared but not defined yet.
    }
};

class C::D::F
{
public:
    static int a;
    static void m(int h)
    {
        D::a = h;
        E::a = h;
    }
};

int main()
{
}
