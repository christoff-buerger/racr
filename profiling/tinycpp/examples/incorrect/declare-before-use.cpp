// This program and the accompanying materials are made available under the
// terms of the MIT license (X11 license) which accompanies this distribution.

// Author: C. Bürger

class C
{
public:
    static int a;
    static void m()
    {
        D::a = a; // D not defined yet.
    }
};

class D
{
public:
    static int a;
    static void m()
    {
        C::a = a;
    }
};

int main()
{
}
