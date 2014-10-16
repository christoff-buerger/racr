// This program and the accompanying materials are made available under the
// terms of the MIT license (X11 license) which accompanies this distribution.

// Author: C. Bürger

class C
{
public:
	class D;
	class E;
	//class C::E // Nested qualified definitions not permitted.
	//{
	//public:
	//};
};

class F
{
public:
	class C::D // Nested qualified definitions not permitted.
	{
	public:
	};
};

int main()
{
}
