/*
 This program and the accompanying materials are made available under the
 terms of the MIT license (X11 license) which accompanies this distribution.

 Author: D. Langner, C. Bürger
*/

using System;
using System.IO;
using System.Windows.Forms;

static class WidgetAttributeMethods {

	static void FormClosed(object sender, EventArgs e) { Application.Exit(); }

	[Racr.AgRule("Widget", "Form")]
	static Control FormWidget(Racr.AstNode n) {
		var form = new System.Windows.Forms.Form();
		form.Text = "Questionnaire";
		var file = new MenuItem("&File");
		var open = new MenuItem("&Open");
		var save = new MenuItem("&Save");
		var quit = new MenuItem("&Quit");
		open.Click += (object sender, EventArgs e) => {
			var ofd = new OpenFileDialog();
			if (ofd.ShowDialog() == DialogResult.OK) {
				var parser = new Parser(QL.Ql, File.ReadAllText(ofd.FileName));
				var ast = parser.ParseAst();
				ast.Render();
				Questionnaire.UpdateQuestions(ast);
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
		return panel;
	}
	[Racr.AgRule("Widget", "Group")]
	static Control GroupWidget(Racr.AstNode n) {
		var panel = new FlowLayoutPanel();
		panel.AutoSize = true;
		panel.BorderStyle = BorderStyle.Fixed3D;
		panel.Dock = DockStyle.Fill;
		panel.FlowDirection = FlowDirection.TopDown;
		panel.WrapContents = false;
		n.Parent().Widget().Controls.Add(panel);
		return panel;
	}
	[Racr.AgRule("Widget", "OrdinaryQuestion")]
	static Control OrdinaryQuestionWidget(Racr.AstNode n) {
		Widget w;
		if (n.Type() == ValueTypes.Boolean) {
			w = new CheckWidget(n.GetLabel());
			var cb = w.GetCheckBox();
			cb.CheckedChanged += (object sender, EventArgs e) => {
				if (!cb.ContainsFocus) return;
				n.SetValue(cb.Checked);
				n.Root().Render();
			};
		}
		else {
			w = new TextWidget(n.GetLabel());
			var tb = w.GetTextBox();
			tb.TextChanged += (object sender, EventArgs e) => {
				if (!tb.ContainsFocus) return;
				if (n.Type() == ValueTypes.Number) {
					try { n.SetValue(Convert.ToDouble(tb.Text)); }
					catch { return; }
				}
				else n.SetValue(tb.Text);
				n.Root().Render();
			};
		}
		n.Parent().Widget().Controls.Add(w);
		return w;
	}
	[Racr.AgRule("Widget", "ComputedQuestion")]
	static Control ComputedQuestionWidget(Racr.AstNode n) {
		Widget w;
		if (n.Type() == ValueTypes.Boolean) w = new CheckWidget(n.GetLabel(), false);
		else w = new TextWidget(n.GetLabel(), false);
		n.Parent().Widget().Controls.Add(w);
		return w;
	}
}

static class RenderAttributeMethods {

	[Racr.AgRule("Render", "Form")]
	static bool FormRender(Racr.AstNode n) {
		n.Widget();
		foreach (var c in n.GetBody().Children()) {
			var child = c as Racr.AstNode;
			var w = child.Widget();
			child.Render();
			if (child.IsShown()) w.Show();
			else w.Hide();
		}
		return true;
	}
	[Racr.AgRule("Render", "Group")]
	static bool GroupRender(Racr.AstNode n) {
		foreach (var c in n.GetBody().Children()) {
			var child = c as Racr.AstNode;
			var w = child.Widget();
			child.Render();
			if (child.IsShown()) w.Show();
			else child.Widget().Hide();
		}
		return true;
	}
	[Racr.AgRule("Render", "OrdinaryQuestion")]
	static bool OrdinaryQuestionRender(Racr.AstNode n) { return false; }
	[Racr.AgRule("Render", "ComputedQuestion")]
	static bool ComputedQuestionRender(Racr.AstNode n) {
		(n.Widget() as Widget).Set(n.Value());
		return true;
	}
}
