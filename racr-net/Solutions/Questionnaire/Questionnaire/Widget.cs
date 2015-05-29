/*
 This program and the accompanying materials are made available under the
 terms of the MIT license (X11 license) which accompanies this distribution.

 Author: D. Langner, C. Bürger
*/

using System;
using System.Windows.Forms;

abstract class Widget : FlowLayoutPanel {
	public Widget(string label) {
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
	private System.Windows.Forms.Label label;
	public virtual TextBox GetTextBox() { return null; }
	public virtual CheckBox GetCheckBox() { return null; }
}
class TextWidget : Widget {
	public TextWidget(string label, bool enabled=true) : base(label) {
		tb = new TextBox();
		tb.Enabled = enabled;
		Controls.Add(tb);
	}
	public override void Set(object v) {
		if (v == null) tb.Text = "";
		else if (v is string) tb.Text = (string) v;
		else tb.Text = Convert.ToString(v);
	}
	public override TextBox GetTextBox() { return tb; }
	private TextBox tb;
}
class CheckWidget : Widget {
	public CheckWidget(string label, bool enabled=true) : base(label) {
		cb = new CheckBox();
		cb.Enabled = enabled;
		Controls.Add(cb);
	}
	public override void Set(object v) {
		cb.Checked = (v == null) ? false : (bool) v;
	}
	public override CheckBox GetCheckBox() { return cb; }
	private CheckBox cb;
}
