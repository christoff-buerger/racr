using System;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;


enum ValueTypes { Boolean, String, Number, ErrorType }


static class Extensions {
	// AST Accessors
	public static Racr.AstNode GetBody(this Racr.AstNode n) { return n.Child("Body"); }
	public static Racr.AstNode GetExpression(this Racr.AstNode n) { return n.Child("Expression"); }
	public static string GetName(this Racr.AstNode n) { return n.Child<string>("name"); }
	public static ValueTypes GetValueType(this Racr.AstNode n) { return n.Child<ValueTypes>("type"); }
	public static object GetValue(this Racr.AstNode n) { return n.Child<object>("value"); }


	// Attribute Accessors
	public static Racr.AstNode Root(this Racr.AstNode n) { return n.AttValue<Racr.AstNode>("Root"); }
	public static Racr.AstNode ErrorQuestion(this Racr.AstNode n) { return n.AttValue<Racr.AstNode>("ErrorQuestion"); }
	public static bool IsErrorQuestion(this Racr.AstNode n) { return n.AttValue<bool>("IsErrorQuestion"); }
	public static Racr.AstNode GLookup(this Racr.AstNode n, string name) { return n.AttValue<Racr.AstNode>("GLookup", name); }
	public static Racr.AstNode LLookup(this Racr.AstNode n, string name) { return n.AttValue<Racr.AstNode>("LLookup", name); }

	public static ValueTypes Type(this Racr.AstNode n) { return n.AttValue<ValueTypes>("Type"); }
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




	static Racr.AstNode findL(string name, Racr.AstNode l, int i) {
		return (Racr.AstNode) l.FindChildA((int j, object e) => {
			return ((Racr.AstNode) e).LLookup(name);
		}, new Racr.Range(1, i));
	}

	static class Form {
		static Racr.AstNode Root(Racr.AstNode n) { return n; }
		static Racr.AstNode ErrorQuestion(Racr.AstNode n) { return n.GetBody().Child(1); }

		[Racr.ContextName("Body")]
		static Racr.AstNode GLookup(Racr.AstNode n, string name) {
			var ret = findL(name, n.Parent(), n.ChildIndex() - 1);
			if (ret != null) return ret;
			return n.ErrorQuestion();
		}
	}

	static class Element {
		static bool IsErrorQuestion(Racr.AstNode n) { return n == n.ErrorQuestion(); }
	}

	static class Group {
		[Racr.ContextName("Body")]
		static Racr.AstNode GLookup(Racr.AstNode n, string name) {
			var ret = findL(name, n.Parent(), n.ChildIndex() - 1);
			if (ret != null) return ret;
			return n.ErrorQuestion();
		}

		static Racr.AstNode LLookup(Racr.AstNode n, string name) {
			return findL(name, n.GetBody(), n.GetBody().NumChildren());
		}
	}

	static class Question {
		static Racr.AstNode LLookup(Racr.AstNode n, string name) {
			if (n.GetName() == name) return n;
			return null;
		}
	}

	static class OrdinaryQuestion {
		static ValueTypes Type(Racr.AstNode n) {
			return n.GetValueType();
		}
	}

	static class ComputedQuestion {
		static ValueTypes Type(Racr.AstNode n) {
			return n.GetExpression().Type();
		}
	}

	static class Use {
		static ValueTypes Type(Racr.AstNode n) {
			return n.GLookup(n.GetName()).Type();
		}
	}

	static class Constant {
		static ValueTypes Type(Racr.AstNode n) {
			var val = n.GetValue();
			if (val is bool) return ValueTypes.Boolean;
			if (val is double) return ValueTypes.Number;
			if (val is string) return ValueTypes.String;
			return ValueTypes.ErrorType;
		}
	}
}



class Questionnaire {

	static QL ql;

	public static void Main() {

		ql = new QL();

	}
}
