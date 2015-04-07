using System;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;



static class Extensions {
	// AST Accessors
	public static Racr.AstNode GetBody(this Racr.AstNode n) { return n.Child("Body"); }
	public static Racr.AstNode GetExpression(this Racr.AstNode n) { return n.Child("Expression"); }
	public static string GetName(this Racr.AstNode n) { return n.Child<string>("name"); }


	// Attribute Accessors
	public static Racr.AstNode Root(this Racr.AstNode n) { return n.AttValue<Racr.AstNode>("Root"); }
	public static Racr.AstNode ErrorQuestion(this Racr.AstNode n) { return n.AttValue<Racr.AstNode>("ErrorQuestion"); }
	public static bool IsErrorQuestion(this Racr.AstNode n) { return n.AttValue<bool>("IsErrorQuestion"); }

}


class QL : Racr.Specification {

	public QL() {
		AstRule("Form->Element*<Body");
		AstRule("Element->");
		AstRule("Group:Element->Expression-Element*<Body");
		AstRule("Question:Element->name-label");
		AstRule("OrdinaryQuestion:Question->type-value");
		AstRule("ComputedQuestion:Question->Expression");
		AstRule("Expression->");
		AstRule("Use:Expression->name");
		AstRule("Constant:Expression->value");
		AstRule("Computation:Expression->operator-Expression*<Operands");
		CompileAstSpecifications("Form");
		CompileAgSpecifications();
	}

	static class Form {
		static Racr.AstNode Root(Racr.AstNode n) { return n; }
		static Racr.AstNode ErrorQuestion(Racr.AstNode n) { return n.GetBody().Child(1); }

		[Racr.ContextName("Body")]
		static Racr.AstNode GLookup(Racr.AstNode n) { return n.GetBody().Child(1); }

	}

	static class Element {
		static bool IsErrorQuestion(Racr.AstNode n) { return n == n.ErrorQuestion(); }
	}


}



class Questionnaire {

	static QL ql;

	public static void Main() {

		ql = new QL();

	}
}
