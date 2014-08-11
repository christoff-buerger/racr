// This program and the accompanying materials are made available under the
// terms of the MIT license (X11 license) which accompanies this distribution.

// Author: C. Bürger

class C
{
public:
	class D;
	class E;
	//class C::E
	//{
	//public:
	//};
};

class F
{
public:
	class C::D
	{
	public:
	};
};

int main()
{
}
