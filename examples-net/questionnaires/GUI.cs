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

static class GUI {
	private static Racr.Specification QL = QuestionnairesLanguage.QL;

	[STAThread] public static void Main(string[] args) {
		Ast ast;
		if (args.Length == 1) {
			var parser = new Parser(QL, File.ReadAllText(args[0]));
			ast = parser.ParseAst();
		} else ast = QL.CreateAst("Form", QL.CreateAstList());
		UpdateQuestions(ast);
		ast.Render();
		Application.Run();
	}

	static void FormClosed(object sender, EventArgs e) { Application.Exit(); }

	static public void UpdateQuestions(Ast n) {
		switch (n.NodeType()) {
		case "Form":
		case "Group":
			foreach (var c in n.GetBody().Children()) UpdateQuestions(c as Ast);
			break;
		case "ComputedQuestion":
			break;
		default:
			(n.Widget() as QWidget).Set(n.Value());
			break;
		}
	}

	// Attribute Accessors:

	public static Control Widget(this Ast n) {
		return n.AttValue<Control>("Widget"); }
	public static bool Render(this Ast n) {
		return n.AttValue<bool>("Render"); }

	// Rendering (GUI):

	[AgRule("Widget", "Form")] static Control FormWidget(Ast n) {
		var form = new System.Windows.Forms.Form();
		form.Text = "Questionnaire";
		var file = new MenuItem("&File");
		var open = new MenuItem("&Open");
		var save = new MenuItem("&Save");
		var quit = new MenuItem("&Quit");
		open.Click += (object sender, EventArgs e) => {
			var ofd = new OpenFileDialog();
			if (ofd.ShowDialog() == DialogResult.OK) {
				var parser = new Parser(QL, File.ReadAllText(ofd.FileName));
				var ast = parser.ParseAst();
				ast.Render();
				UpdateQuestions(ast);
				form.Closed -= FormClosed;
				form.Close();
			}
		};
		save.Click += (object sender, EventArgs e) => {
			var sfd = new SaveFileDialog();
			if (sfd.ShowDialog() == DialogResult.OK && sfd.FileName != "") {
				File.WriteAllText(sfd.FileName, n.SExpr());
			}
		};
		quit.Click += (object sender, EventArgs e) => form.Close();
		form.Closed += FormClosed;

		file.MenuItems.Add(open);
		file.MenuItems.Add(save);
		file.MenuItems.Add(quit);
		form.Menu = new MainMenu();
		form.Menu.MenuItems.Add(file);

		var panel = new FlowLayoutPanel();
		panel.AutoSize = true;
		panel.AutoScroll = true;
		panel.Dock = DockStyle.Fill;
		panel.FlowDirection = FlowDirection.TopDown;
		panel.WrapContents = false;
		form.Controls.Add(panel);
		form.Show();
		return panel; }
	[AgRule("Widget", "Group")] static Control GroupWidget(Ast n) {
		var panel = new FlowLayoutPanel();
		panel.AutoSize = true;
		panel.BorderStyle = BorderStyle.Fixed3D;
		panel.Dock = DockStyle.Fill;
		panel.FlowDirection = FlowDirection.TopDown;
		panel.WrapContents = false;
		n.Parent().Widget().Controls.Add(panel);
		return panel; }
	[AgRule("Widget", "ComputedQuestion")] static Control ComputedQuestionWidget(Ast n) {
		QWidget w;
		if (n.Type() == Types.Boolean) w = new QCheckWidget(n.GetLabel(), false);
		else w = new QTextWidget(n.GetLabel(), false);
		n.Parent().Widget().Controls.Add(w);
		return w; }
	[AgRule("Widget", "OrdinaryQuestion")] static Control OrdinaryQuestionWidget(Ast n) {
		QWidget w;
		if (n.Type() == Types.Boolean) {
			w = new QCheckWidget(n.GetLabel());
			var cb = w.GetCheckBox();
			cb.CheckedChanged += (object sender, EventArgs e) => {
				if (!cb.ContainsFocus) return;
				n.SetValue(cb.Checked);
				n.Root().Render();
			};
		} else {
			w = new QTextWidget(n.GetLabel());
			var tb = w.GetTextBox();
			tb.TextChanged += (object sender, EventArgs e) => {
				if (!tb.ContainsFocus) return;
				if (n.Type() == Types.Number) {
					try { n.SetValue(Convert.ToDouble(tb.Text)); }
					catch { return; }
				} else n.SetValue(tb.Text);
				n.Root().Render();
			};
		}
		n.Parent().Widget().Controls.Add(w);
		return w; }

	[AgRule("Render", "OrdinaryQuestion")] static bool OrdinaryQuestionRender(Ast n) {
		return false; }
	[AgRule("Render", "Form")] static bool FormRender(Ast n) {
		n.Widget();
		foreach (Ast c in n.GetBody().Children()) {
			if (c.IsShown()) {
				c.Render();
				c.Widget().Show();
			} else c.Widget().Hide();
		}
		return true; }
	[AgRule("Render", "Group")] static bool GroupRender(Ast n) {
		foreach (Ast c in n.GetBody().Children()) {
			if (c.IsShown()) {
				c.Render();
				c.Widget().Show();
			} else c.Widget().Hide();
		}
		return true; }
	[AgRule("Render", "ComputedQuestion")] static bool ComputedQuestionRender(Ast n) {
		(n.Widget() as QWidget).Set(n.Value());
		return true; }

	private abstract class QWidget : FlowLayoutPanel {
		private System.Windows.Forms.Label label;
		public QWidget(string label) {
			AutoSize = true;
			WrapContents = false;
			FlowDirection = FlowDirection.LeftToRight;
			this.label = new System.Windows.Forms.Label();
			this.label.Text = label;
			this.label.AutoSize = true;
			this.label.Anchor = AnchorStyles.Left;
			Controls.Add(this.label);
		}
		public abstract void Set(object v);
		public virtual TextBox GetTextBox() { return null; }
		public virtual CheckBox GetCheckBox() { return null; }
	}

	private class QTextWidget : QWidget {
		private TextBox tb;
		public QTextWidget(string label, bool enabled=true) : base(label) {
			tb = new TextBox();
			tb.Enabled = enabled;
			Controls.Add(tb);
		}
		public override TextBox GetTextBox() { return tb; }
		public override void Set(object v) {
			if (v == null) tb.Text = "";
			else if (v is string) tb.Text = (string) v;
			else tb.Text = Convert.ToString(v);
		}
	}

	private class QCheckWidget : QWidget {
		private CheckBox cb;
		public QCheckWidget(string label, bool enabled=true) : base(label) {
			cb = new CheckBox();
			cb.Enabled = enabled;
			Controls.Add(cb);
		}
		public override CheckBox GetCheckBox() { return cb; }
		public override void Set(object v) {
			cb.Checked = (v == null) ? false : (bool) v;
		}
	}
}
