// This program and the accompanying materials are made available under the
// terms of the MIT license (X11 license) which accompanies this distribution.

// Author: C. Bürger

class C
{
public:
	class D
	{
	public:
		static int a;
		//class C
		//{
		//public:
		//	static int a;
		//	static void m(int h)
		//	{
		//		C::D::C::a = h;  // C::D::C not declared. Resolution of such name impossible because of shortcut evaluation.
		//	}
		//};
	};
	static int a;
	static void m(int h)
	{
		E::a = h;
	}
	class E
	{
	public:
		static int a;
		static void m(int h)
		{
			D::a = h;
		}
		class F
		{
		public:
			static void m()
			{
				D::a = E::a;
			}
		};
	};
};

// Errors only detected by linker:

//class m
//{
//	public:
//		static int a;
//};

//class G
//{
//public:
//	static void m(int h) // If ever called fails with linker error
//	{
//		m::a = h;
//	}
//	static void m2(int h) // If ever called fails with linker error
//	{
//		m2::a = h;
//	}
//	class m2
//	{
//	public:
//		static int a;
//	};
//};

class H
{
public:
	static int a;
	class I
	{
	public:
		static void H(int h)
		{
			//H::a = h; // If ever called fails with linker error
			J::a = h; // If ever called fails with linker error
		}
		static void J(int h)
		{
			//H::a = h; // If ever called fails with linker error
		}
	};
	class J
	{
	public:
		static int a;
	};
};

int main()
{
	//class G::m2 o; // Ok
	//G::m(1); // Linker error
	//G::m2(1); // Linker error
	//H::I::H(1); // Linker error
	//H::I::J(1); // Linker error
}
