// This program and the accompanying materials are made available under the
// terms of the MIT license (X11 license) which accompanies this distribution.

// Author: C. Bürger

class C;
//class C; // Redeclaration.

class C
{
public:
    class D;
    //class D; // Redeclaration
    class D
    {
    public:
    };
    //class D // Redefinition.
    //{
    //public:
    //};
    class E;
};

class C // Redefinition.
{
public:
};

class C::E
{
public:
};

//class C::E // Redefinition
//{
//public:
//};

int main()
{
}
