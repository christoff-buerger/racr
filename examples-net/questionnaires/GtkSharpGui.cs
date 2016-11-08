/*
 This program and the accompanying materials are made available under the
 terms of the MIT license (X11 license) which accompanies this distribution.

 Author: C. Bürger
*/

using System;
using System.IO;
using Gtk;

using Ast = Racr.AstNode;
using AgRule = Racr.AgRuleAttribute;
using Types = QuestionnairesLanguage.Types;

static class QuestionnairesGui {
	private static Racr.Specification QL = QuestionnairesLanguage.QL;

	[STAThread] public static void Main(string[] args) {
		Application.Init();
		if (OpenQuestionnaire(null)) Application.Run();
	}

	private static void UpdateQuestions(Ast n) {
		switch (n.NodeType()) {
		case "Form": case "Group":
			foreach (var c in n.GetBody().Children()) UpdateQuestions(c as Ast);
			break;
		case "ComputedQuestion":
			break;
		default:
			n.Dialog().SetValue(n.Value());
			break;
		}
	}
 
	private static bool OpenQuestionnaire(Ast current) {
		FileChooserDialog fc = new FileChooserDialog(
			"Select questionnaire to open",
			null,
			FileChooserAction.Open,
			"Cancel", ResponseType.Cancel,
			"Open questionnaire", ResponseType.Accept);
		fc.SetPosition(WindowPosition.Center);
		Ast next = null;
		while (fc.Run() == (int)ResponseType.Accept) {
			try {
				var parser = new Parser(QL, File.ReadAllText(fc.Filename));
				next = parser.ParseAst();
				break;
			} catch { }
		}
		fc.Destroy();
		if (next != null) {
			if (current != null) current.Widget().Toplevel.Destroy();
			UpdateQuestions(next);
			next.Render();
			return true;
		} else return false;
	}

	private static void SaveQuestionnaire(Ast current) {
		FileChooserDialog fc = new FileChooserDialog(
			"Select where to save questionnaire",
			null,
			FileChooserAction.Save,
			"Cancel", ResponseType.Cancel,
			"Save questionnaire", ResponseType.Accept);
		fc.SetPosition(WindowPosition.Center);
		if (fc.Run() == (int)ResponseType.Accept) {
			File.WriteAllText(fc.Filename, current.SExpr());
		}
		fc.Destroy();
	}

	// Attribute Accessors:

	static QDialog Dialog(this Ast n)	{ return n.AttValue<QDialog>("Dialog"); }
	static Box Widget(this Ast n)		{ return n.AttValue<Box>("Widget"); }
	static bool Render(this Ast n)		{ return n.AttValue<bool>("Render"); }

	// Widgets:

	[AgRule("Widget", "Form")] static Box FormWidget(Ast n) {
		Window window = new Window("Questionnaire");
		window.SetDefaultSize(300, 600);
		window.DeleteEvent += (object o, DeleteEventArgs a) => { Application.Quit(); };

		VBox contentBox = new VBox(false, 5);
		ScrolledWindow scroller = new ScrolledWindow();
		scroller.AddWithViewport(contentBox);

		Toolbar toolbar = new Toolbar();
		toolbar.ToolbarStyle = ToolbarStyle.Icons;
		ToolButton open = new ToolButton(Stock.Open);
		ToolButton save = new ToolButton(Stock.Save);
		ToolButton quit = new ToolButton(Stock.Quit);
		open.Clicked += (object o, EventArgs a) => { OpenQuestionnaire(n); };
		save.Clicked += (object o, EventArgs a) => { SaveQuestionnaire(n); };
		quit.Clicked += (object o, EventArgs a) => { Application.Quit(); };
		toolbar.Insert(open, 0);
		toolbar.Insert(save, 1);
		toolbar.Insert(quit, 2);

		VBox windowBox = new VBox(false, 0);
		windowBox.PackStart(toolbar, false, false, 0);
		if (!n.IsValid()) {
			Image warning = new Image(Stock.DialogWarning, IconSize.Dialog);
			windowBox.PackStart(warning, false, false, 0);
		}
		windowBox.PackStart(scroller, true, true, 0);

		window.Add(windowBox);
		window.ShowAll();
		return contentBox;
	}

	[AgRule("Widget", "Group")] static Box GroupWidget(Ast n) {
		VBox contentBox = new VBox(false, 5);
		Frame frame = new Frame(n.GetExpression().SExpr());
		frame.BorderWidth = 5;
		frame.Add(contentBox);
		if (!n.IsLValid()) {
			Gdk.Color col = new Gdk.Color();
			Gdk.Color.Parse("red", ref col);
			frame.ModifyBg(StateType.Normal, col);
		}
		n.Parent().Widget().PackStart(frame, false, false, 5);
		frame.Show();
		return contentBox;
	}

	[AgRule("Widget", "ComputedQuestion")] static Box ComputedQuestionWidget(Ast n) {
		Widget dialog = (Widget) n.Dialog();
		dialog.Sensitive = false;
		HBox box = new HBox(false, 5);
		if (!n.IsLValid()) {
			Image warning = new Image(Stock.DialogWarning, IconSize.Button);
			box.PackStart(warning, false, false, 3);
		}
		box.PackStart(new Label(n.GetLabel()), false, false, 5);
		box.PackStart(dialog, true, true, 5);
		n.Parent().Widget().PackStart(box, false, false, 5);
		box.ShowAll();
		return box;
	}

	[AgRule("Widget", "OrdinaryQuestion")] static Box OrdinaryQuestionWidget(Ast n) {
		QDialog dialog = n.Dialog();
		EventHandler h = (object o, EventArgs a) => {
			n.SetValue(dialog.GetValue());
			n.Root().Render();
		};
		dialog.AddEventHandler(h);
		HBox box = new HBox(false, 5);
		if (!n.IsLValid()) {
			Image warning = new Image(Stock.DialogWarning, IconSize.Button);
			box.PackStart(warning, false, false, 3);
		}
		box.PackStart(new Label(n.GetLabel()), false, false, 5);
		box.PackStart((Widget)dialog, true, true, 5);
		n.Parent().Widget().PackStart(box, false, false, 5);
		box.ShowAll();
		return box;
	}

	// Rendering:

	[AgRule("Render", "OrdinaryQuestion")] static bool OrdinaryQuestionRender(Ast n) {
		return true;
	}

	[AgRule("Render", "ComputedQuestion")] static bool ComputedQuestionRender(Ast n) {
		n.Dialog().SetValue(n.Value());
		return true;
	}

	[AgRule("Render", "Form")] static bool FormRender(Ast n) {
		foreach (Ast c in n.GetBody().Children()) {
			if (c.IsShown()) {
				c.Render();
				c.Widget().Show();
			} else c.Widget().Hide();
		}
		n.Widget().Show();
		return true;
	}

	[AgRule("Render", "Group")] static bool GroupRender(Ast n) {
		foreach (Ast c in n.GetBody().Children()) {
			if (c.IsShown()) {
				c.Render();
				c.Widget().Show();
			} else c.Widget().Hide();
		}
		n.Widget().Show();
		return true;
	}

	// Question dialogs:
	
	[AgRule("Dialog", "Question")] static QDialog QuestionDialogType(Ast n) {
		switch (n.Type()) {
		case Types.Boolean:
			return new QCheckBoxDialog();
		case Types.Number: case Types.String: case Types.ErrorType:
			return new QTextFieldDialog(n);
		default:
			throw new NotImplementedException(
				"No dialog for [" + n.Type() + "] implemented.");
		}
	}

	private interface QDialog { // All QDialog implementations must inherit from Gtk.Widget
		void SetValue(object o);
		object GetValue();
		void AddEventHandler(EventHandler h);
	}

	private class QCheckBoxDialog : CheckButton, QDialog {
		public QCheckBoxDialog() { }
		public void SetValue(object v) { Active = v is bool ? (bool) v : false; }
		public object GetValue() { return Active; }
		public void AddEventHandler(EventHandler h) { Clicked += h; }
	}

	private class QTextFieldDialog : Entry, QDialog {
		private Ast n;

		public QTextFieldDialog(Ast question) {
			n = question;
		}

		public void SetValue(object v) {
			var acceptor = QuestionnairesLanguage.TypeToAcceptor(n.Type());
			switch (n.Type()) {
			case Types.String: case Types.Number:
				Text = acceptor(v) ? Convert.ToString(v) : "";
				break;
			case Types.ErrorType:
				Text = "";
				break;
			default:
				throw new NotImplementedException(
					"No [" + n.Type() + "] printer for [QTextDialog] implemented.");
			}
		}

		public object GetValue() {
			if (n.Type() == Types.Number) {
				try { return Convert.ToDouble(Text); }
				catch { return Text; }
			} else return Text;
		}

		public void AddEventHandler(EventHandler h) {
			Changed += h;
		}
	}
}
