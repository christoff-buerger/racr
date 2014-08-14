// This program and the accompanying materials are made available under the
// terms of the MIT license (X11 license) which accompanies this distribution.

// Author: C. Bürger

class C
{
public:
	static int a;
};

class D
{
public:
	static int a;
	static void m1(int E)
	{
	}
	static void m2(int C, int D)
	{
		a = C;
		C::a = C;
		D::a = D;
		a = E; // E not declared.
	}
};

int main()
{
}
