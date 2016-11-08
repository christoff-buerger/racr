/*
 This program and the accompanying materials are made available under the
 terms of the MIT license (X11 license) which accompanies this distribution.

 Author: D. Langner, C. Bürger
*/

using System;
using System.IO;
using System.Windows.Forms;

using Ast = Racr.AstNode;
using AgRule = Racr.AgRuleAttribute;
using Types = QuestionnairesLanguage.Types;

static class QuestionnairesGui {
	private static Racr.Specification QL = QuestionnairesLanguage.QL;

	[STAThread] public static void Main(string[] args) {
		if (OpenQuestionnaire()) Application.Run();
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

	private static bool OpenQuestionnaire() {
		var fd = new OpenFileDialog();
		Ast next = null;
		while (fd.ShowDialog() == DialogResult.OK) {
			try {
				var parser = new Parser(QL, File.ReadAllText(fd.FileName));
				next = parser.ParseAst();
				break;
			} catch { }
		}
		fd.Dispose();
		if (next != null) {
			UpdateQuestions(next);
			next.Render();
			return true;
		} else return false;
	}

	private static void SaveQuestionnaire(Ast current) {
		var fd = new SaveFileDialog();
		if (fd.ShowDialog() == DialogResult.OK && fd.FileName != "") {
			File.WriteAllText(fd.FileName, current.SExpr());
		}
		fd.Dispose();
	}

	// Attribute Accessors:

	static QDialog Dialog(this Ast n)	{ return n.AttValue<QDialog>("Dialog"); }
	static Control Widget(this Ast n)	{ return n.AttValue<Control>("Widget"); }
	static bool Render(this Ast n)		{ return n.AttValue<bool>("Render"); }

	// Widgets:

	[AgRule("Widget", "Form")] static Control FormWidget(Ast n) {
		var form = new System.Windows.Forms.Form();
		form.Text = "Questionnaire";
		form.Closed += (object sender, EventArgs e) => { Application.Exit(); };

		var file = new MenuItem("&File");
		var save = new MenuItem("&Save");
		var quit = new MenuItem("&Quit");
		save.Click += (object sender, EventArgs e) => { SaveQuestionnaire(n); };
		quit.Click += (object sender, EventArgs e) => { Application.Exit(); };
		file.MenuItems.Add(save);
		file.MenuItems.Add(quit);
		form.Menu = new MainMenu();
		form.Menu.MenuItems.Add(file);

		var contentBox = new FlowLayoutPanel();
		contentBox.AutoSize = true;
		contentBox.AutoScroll = true;
		contentBox.Dock = DockStyle.Fill;
		contentBox.FlowDirection = FlowDirection.TopDown;
		contentBox.WrapContents = false;
		form.Controls.Add(contentBox);
		form.Show();
		return contentBox;
	}

	[AgRule("Widget", "Group")] static Control GroupWidget(Ast n) {
		var contentBox = new FlowLayoutPanel();
		contentBox.AutoSize = true;
		contentBox.BorderStyle = BorderStyle.Fixed3D;
		contentBox.Dock = DockStyle.Fill;
		contentBox.FlowDirection = FlowDirection.TopDown;
		contentBox.WrapContents = false;
		n.Parent().Widget().Controls.Add(contentBox);
		return contentBox;
	}

	[AgRule("Widget", "ComputedQuestion")] static Control ComputedQuestionWidget(Ast n) {
		Control dialog = (Control) n.Dialog();		
		dialog.Enabled = false;

		var contentBox = new FlowLayoutPanel();
		contentBox.AutoSize = true;
		contentBox.WrapContents = false;
		contentBox.FlowDirection = FlowDirection.LeftToRight;
		var label = new Label();
		label.Text = n.GetLabel();
		label.AutoSize = true;
		label.Anchor = AnchorStyles.Left;
		contentBox.Controls.Add(label);
		contentBox.Controls.Add(dialog);

		n.Parent().Widget().Controls.Add(contentBox);
		return contentBox;
	}

	[AgRule("Widget", "OrdinaryQuestion")] static Control OrdinaryQuestionWidget(Ast n) {
		QDialog dialog = n.Dialog();		
		((Control)dialog).Enabled = true;
		EventHandler h = (object o, EventArgs a) => {
			if (!((Control)o).ContainsFocus) return; // TODO: For which reason is this check needed?
			n.SetValue(dialog.GetValue());
			n.Root().Render();
		};
		dialog.AddEventHandler(h);

		var contentBox = new FlowLayoutPanel();
		contentBox.AutoSize = true;
		contentBox.WrapContents = false;
		contentBox.FlowDirection = FlowDirection.LeftToRight;
		var label = new Label();
		label.Text = n.GetLabel();
		label.AutoSize = true;
		label.Anchor = AnchorStyles.Left;
		contentBox.Controls.Add(label);
		contentBox.Controls.Add((Control)dialog);

		n.Parent().Widget().Controls.Add(contentBox);
		return contentBox;
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

	private interface QDialog { // All QDialog implementations must inherit from Control
		void SetValue(object o);
		object GetValue();
		void AddEventHandler(EventHandler h);
	}

	private class QCheckBoxDialog : CheckBox, QDialog {
		public QCheckBoxDialog() { }	
		public void SetValue(object v) { Checked = v is bool ? (bool) v : false; }
		public object GetValue() { return Checked; }
		public void AddEventHandler(EventHandler h) { CheckedChanged += h; }
	}

	private class QTextFieldDialog : TextBox, QDialog {
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
			TextChanged += h;
		}
	}
}
