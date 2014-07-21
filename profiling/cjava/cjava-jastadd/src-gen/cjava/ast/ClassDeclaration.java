/* This file was generated with JastAdd2 (http://jastadd.org) version 2.1.8 */
package cjava.ast;

import java.util.regex.*;
import java.io.*;
/**
 * @ast node
 * @declaredat specifications/AST.ast:24
 * @production ClassDeclaration : {@link Declaration} ::= <span class="component">Body:{@link Declaration}*</span> <span class="component">&lt;Source:String&gt;</span>;

 */
public class ClassDeclaration extends Declaration implements Cloneable {
  /**
   * @aspect Unparser
   * @declaredat specifications/Unparser.jrag:18
   */
  public void pp(Writer writer, int indent) throws IOException {
		printIdent(writer, indent);
		if (getParent().getParent() instanceof CompilationUnit)
			writer.write("public class " + getName() + " {\n");
		else
			writer.write("public static class " + getName() + " {\n");
		for (Declaration d:getBodyList())
			d.pp(writer, indent + 1);
		printIdent(writer, indent);
		writer.write("}\n");
	}
  /**
   * @declaredat ASTNode:1
   */
  public ClassDeclaration() {
    super();
  }
  /**
   * Initializes the child array to the correct size.
   * Initializes List and Opt nta children.
   * @apilevel internal
   * @ast method
   * @declaredat ASTNode:10
   */
  public void init$Children() {
    children = new ASTNode[1];
    state().enterConstruction();
    setChild(new List(), 0);
    state().exitConstruction();
  }
  /**
   * @declaredat ASTNode:16
   */
  public ClassDeclaration(String p0, List<Declaration> p1, String p2) {
state().enterConstruction();
    setName(p0);
    setChild(p1, 0);
    setSource(p2);
state().exitConstruction();
  }
  /**
   * @apilevel low-level
   * @declaredat ASTNode:26
   */
  protected int numChildren() {
    
    
    state().addHandlerDepTo(numChildren_handler);
    return 1;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:35
   */
  public boolean mayHaveRewrite() {
    return false;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:41
   */
  public void flushAttrCache() {
    super.flushAttrCache();
    lookup_String_reset();
    localLookup_String_reset();
    isCorrect_reset();
    lookupRef_String_reset();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:51
   */
  public void flushCollectionCache() {
    super.flushCollectionCache();
  }
  /**
   * @api internal
   * @declaredat ASTNode:57
   */
  public void flushRewriteCache() {
    super.flushRewriteCache();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:63
   */
  public ClassDeclaration clone() throws CloneNotSupportedException {
    ClassDeclaration node = (ClassDeclaration) super.clone();
    return node;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:70
   */
  public ClassDeclaration copy() {
    try {
      ClassDeclaration node = (ClassDeclaration) clone();
      node.parent = null;
      if(children != null) {
        node.children = (ASTNode[]) children.clone();
      }
      node.inc_state = inc_CLONED;
      for (int i = 0; node.children != null && i < node.children.length; i++) {
        node.children[i] = null;
      }
      node.init_children = null;
      node.children_computed = null;
      inc_copyHandlers(node);
      return node;
    } catch (CloneNotSupportedException e) {
      throw new Error("Error: clone not supported for " + getClass().getName());
    }
  }
  /**
   * Create a deep copy of the AST subtree at this node.
   * The copy is dangling, i.e. has no parent.
   * @return dangling copy of the subtree at this node
   * @apilevel low-level
   * @deprecated Please use emitTreeCopy or emitTreeCopyNoTransform instead
   * @declaredat ASTNode:96
   */
  public ClassDeclaration fullCopy() {
    return treeCopyNoTransform();
  }
  /**
   * Create a deep copy of the AST subtree at this node.
   * The copy is dangling, i.e. has no parent.
   * @return dangling copy of the subtree at this node
   * @apilevel low-level
   * @declaredat ASTNode:105
   */
  public ClassDeclaration treeCopyNoTransform() {
    ClassDeclaration tree = (ClassDeclaration) copy();
    if (children != null) {
      for (int i = 0; i < children.length; ++i) {
        ASTNode child = (ASTNode) children[i];
        if(child != null) {
          child = child.treeCopyNoTransform();
          tree.children[i] = child;
          child.parent = tree;
        }
      }
    }
    return tree;
  }
  /**
   * Create a deep copy of the AST subtree at this node.
   * The subtree of this node is traversed to trigger rewrites before copy.
   * The copy is dangling, i.e. has no parent.
   * @return dangling copy of the subtree at this node
   * @apilevel low-level
   * @declaredat ASTNode:126
   */
  public ClassDeclaration treeCopy() {
    doFullTraversal();
    return treeCopyNoTransform();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:133
   */
  protected boolean is$Equal(ASTNode node) {
    return super.is$Equal(node)  && (tokenString_Name == ((ClassDeclaration)node).tokenString_Name) && (tokenString_Source == ((ClassDeclaration)node).tokenString_Source);    
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:139
   */
  protected boolean childIsNTA(int index) {
    return super.childIsNTA(index);
  }
  /**
   * @declaredat ASTNode:142
   */
  protected ASTNode$DepGraphNode lookup_String_handler;
  /**
   * @declaredat ASTNode:143
   */
  protected ASTNode$DepGraphNode localLookup_String_handler;
  /**
   * @declaredat ASTNode:144
   */
  protected ASTNode$DepGraphNode lookupRef_String_handler;
  /**
   * @declaredat ASTNode:145
   */
  protected void inc_copyHandlers(ClassDeclaration copy) {
    super.inc_copyHandlers(copy);

        if (getSource_handler != null) {
          copy.getSource_handler = new ASTNode$DepGraphNode(getSource_handler, copy);
        }
        if (lookup_String_handler != null) {
          copy.lookup_String_handler = new ASTNode$DepGraphNode(lookup_String_handler, copy);
        }
        if (localLookup_String_handler != null) {
          copy.localLookup_String_handler = new ASTNode$DepGraphNode(localLookup_String_handler, copy);
        }
        if (lookupRef_String_handler != null) {
          copy.lookupRef_String_handler = new ASTNode$DepGraphNode(lookupRef_String_handler, copy);
        }
  }
  /**
   * @api internal
   * @declaredat ASTNode:166
   */
  public void reactToDependencyChange(String attrID) {
    
    // flush children
    
    
    
    
    
    if (attrID.equals("lookup_String") && lookup_String_values != null && !lookup_String_values.isEmpty()) {
      lookup_String_values = null;
      lookup_String_handler.notifyDependencies();
      super.reactToDependencyChange(attrID);
      return;
    }
    else 
    if (attrID.equals("localLookup_String") && localLookup_String_values != null && !localLookup_String_values.isEmpty()) {
      localLookup_String_values = null;
      localLookup_String_handler.notifyDependencies();
      super.reactToDependencyChange(attrID);
      return;
    }
    else 
    if (attrID.equals("isCorrect") && isCorrect_computed) {
      isCorrect_computed = false;
      isCorrect_handler.notifyDependencies();
      super.reactToDependencyChange(attrID);
      return;
    }
    else 
    if (attrID.equals("lookupRef_String") && lookupRef_String_values != null && !lookupRef_String_values.isEmpty()) {
      lookupRef_String_values = null;
      lookupRef_String_handler.notifyDependencies();
      super.reactToDependencyChange(attrID);
      return;
    }
    super.reactToDependencyChange(attrID);
  }
  /**
   * @api internal
   * @declaredat ASTNode:212
   */
  protected void inc_flush_subtree(ASTNode$DepGraphNode h) {
    inc_state = inc_GARBAGE;
    h.transferSetsFrom(lookup_String_handler);
    
    h.transferSetsFrom(localLookup_String_handler);
    
    h.transferSetsFrom(isCorrect_handler);
    
    h.transferSetsFrom(lookupRef_String_handler);
    
    h.transferSetsFrom(getSource_handler);
    super.inc_flush_subtree(h);
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:231
   */
  public void inc_changeState(int newState) {
  super.inc_changeState(newState);
  if (getSource_handler != null) {
    getSource_handler.changeState(newState);
  }
  if (lookup_String_handler != null) {
    lookup_String_handler.changeState(newState);
  }
  if (localLookup_String_handler != null) {
    localLookup_String_handler.changeState(newState);
  }
  if (isCorrect_handler != null) {
    isCorrect_handler.changeState(newState);
  }
  if (lookupRef_String_handler != null) {
    lookupRef_String_handler.changeState(newState);
  }
}
  /**
   * @apilevel internal
   * @declaredat ASTNode:252
   */
  public void inc_throwAway() {
  inc_state = inc_GARBAGE;
  super.inc_throwAway();
  if (getSource_handler != null) {
    getSource_handler.throwAway();
  }
  if (lookup_String_handler != null) {
    lookup_String_handler.throwAway();
  }
  if (localLookup_String_handler != null) {
    localLookup_String_handler.throwAway();
  }
  if (isCorrect_handler != null) {
    isCorrect_handler.throwAway();
  }
  if (lookupRef_String_handler != null) {
    lookupRef_String_handler.throwAway();
  }
}
  /**
   */
  protected ASTNode$DepGraphNode getName_handler = new ASTNode$DepGraphNode(this, "getName");
  /**
   * Replaces the lexeme Name.
   * @param value The new value for the lexeme Name.
   * @apilevel high-level
   */
  public void setName(String value) {
    tokenString_Name = value;
    
    
    if (!state().IN_CONSTRUCTION && !state().IN_ATTR_STORE_EVAL) {
      getName_handler.notifyDependencies();
    
    
    
      ASTNode initial = inc_locateInitialCopy();
      if (initial != null) {
        state().enterConstruction();
        ((ClassDeclaration)initial).setName(tokenString_Name);
        state().exitConstruction();
        return;
      }
    
    }
  }
  /**
   * Retrieves the value for the lexeme Name.
   * @return The value for the lexeme Name.
   * @apilevel high-level
   */
  @ASTNodeAnnotation.Token(name="Name")
  public String getName() {
    
    
    state().addHandlerDepTo(getName_handler);
    return tokenString_Name != null ? tokenString_Name : "";
  }
  /**
   * Replaces the Body list.
   * @param list The new list node to be used as the Body list.
   * @apilevel high-level
   */
  public void setBodyList(List<Declaration> list) {
    state().enterConstruction();
    setChild(list, 0);
    state().exitConstruction();
  }
  /**
   * Retrieves the number of children in the Body list.
   * @return Number of children in the Body list.
   * @apilevel high-level
   */
  public int getNumBody() {
    return getBodyList().getNumChild();
  }
  /**
   * Retrieves the number of children in the Body list.
   * Calling this method will not trigger rewrites.
   * @return Number of children in the Body list.
   * @apilevel low-level
   */
  public int getNumBodyNoTransform() {
    return getBodyListNoTransform().getNumChildNoTransform();
  }
  /**
   * Retrieves the element at index {@code i} in the Body list.
   * @param i Index of the element to return.
   * @return The element at position {@code i} in the Body list.
   * @apilevel high-level
   */
  public Declaration getBody(int i) {
    return (Declaration) getBodyList().getChild(i);
  }
  /**
   * Check whether the Body list has any children.
   * @return {@code true} if it has at least one child, {@code false} otherwise.
   * @apilevel high-level
   */
  public boolean hasBody() {
    return getBodyList().getNumChild() != 0;
  }
  /**
   * Append an element to the Body list.
   * @param node The element to append to the Body list.
   * @apilevel high-level
   */
  public void addBody(Declaration node) {
    List<Declaration> list = (parent == null || state == null) ? getBodyListNoTransform() : getBodyList();
    list.addChild(node);
  }
  /**
   * @apilevel low-level
   */
  public void addBodyNoTransform(Declaration node) {
    List<Declaration> list = getBodyListNoTransform();
    list.addChild(node);
  }
  /**
   * Replaces the Body list element at index {@code i} with the new node {@code node}.
   * @param node The new node to replace the old list element.
   * @param i The list index of the node to be replaced.
   * @apilevel high-level
   */
  public void setBody(Declaration node, int i) {
    List<Declaration> list = getBodyList();
    state().enterConstruction();
    list.setChild(node, i);
    state().exitConstruction();
  }
  /**
   * Retrieves the Body list.
   * @return The node representing the Body list.
   * @apilevel high-level
   */
  @ASTNodeAnnotation.ListChild(name="Body")
  public List<Declaration> getBodyList() {
    List<Declaration> list = (List<Declaration>) getChild(0);
    list.getNumChild();
    return list;
  }
  /**
   * Retrieves the Body list.
   * <p><em>This method does not invoke AST transformations.</em></p>
   * @return The node representing the Body list.
   * @apilevel low-level
   */
  public List<Declaration> getBodyListNoTransform() {
    return (List<Declaration>) getChildNoTransform(0);
  }
  /**
   * Retrieves the Body list.
   * @return The node representing the Body list.
   * @apilevel high-level
   */
  public List<Declaration> getBodys() {
    return getBodyList();
  }
  /**
   * Retrieves the Body list.
   * <p><em>This method does not invoke AST transformations.</em></p>
   * @return The node representing the Body list.
   * @apilevel low-level
   */
  public List<Declaration> getBodysNoTransform() {
    return getBodyListNoTransform();
  }
  /**
   */
  protected ASTNode$DepGraphNode getSource_handler = new ASTNode$DepGraphNode(this, "getSource");
  /**
   * Replaces the lexeme Source.
   * @param value The new value for the lexeme Source.
   * @apilevel high-level
   */
  public void setSource(String value) {
    tokenString_Source = value;
    
    
    if (!state().IN_CONSTRUCTION && !state().IN_ATTR_STORE_EVAL) {
      getSource_handler.notifyDependencies();
    
    
    
      ASTNode initial = inc_locateInitialCopy();
      if (initial != null) {
        state().enterConstruction();
        ((ClassDeclaration)initial).setSource(tokenString_Source);
        state().exitConstruction();
        return;
      }
    
    }
  }
  /**
   * @apilevel internal
   */
  protected String tokenString_Source;
  /**
   * Retrieves the value for the lexeme Source.
   * @return The value for the lexeme Source.
   * @apilevel high-level
   */
  @ASTNodeAnnotation.Token(name="Source")
  public String getSource() {
    
    
    state().addHandlerDepTo(getSource_handler);
    return tokenString_Source != null ? tokenString_Source : "";
  }
  protected java.util.Map lookup_String_values;
/**
 * @apilevel internal
 */
private void lookup_String_reset() {
  lookup_String_values = null;
}  
  @ASTNodeAnnotation.Attribute
  public Declaration lookup(String label) {
    Object _parameters = label;
    if (lookup_String_values == null) lookup_String_values = new java.util.HashMap(4);
    
    
    if (lookup_String_handler == null) {
      lookup_String_handler = new ASTNode$DepGraphNode(this, "lookup_String");
    }
    state().addHandlerDepTo(lookup_String_handler);
    
    
    
    
    if(lookup_String_values.containsKey(_parameters)) {
      return (Declaration)lookup_String_values.get(_parameters);
    }
    
    
    ASTNode$DepGraphNode tmpHandler = new ASTNode$DepGraphNode(this, "");
    state().enterAttrStoreEval(tmpHandler);
    ASTNode$State state = state();
    boolean intermediate = state.INTERMEDIATE_VALUE;
    state.INTERMEDIATE_VALUE = false;
    int num = state.boundariesCrossed;
    boolean isFinal = this.is$Final();
    Declaration lookup_String_value = doLookup(this, label);
    if (isFinal && num == state().boundariesCrossed) {
      
      
      lookup_String_handler.transferDependenciesFrom(tmpHandler);
      lookup_String_values.put(_parameters, lookup_String_value);
    } else {
      
      
      tmpHandler.clearDependencies();
    }
    state.INTERMEDIATE_VALUE |= intermediate;

    
    
    state().exitAttrStoreEval(tmpHandler);
    
    
    
    
    return lookup_String_value;
  }
  protected java.util.Map localLookup_String_values;
/**
 * @apilevel internal
 */
private void localLookup_String_reset() {
  localLookup_String_values = null;
}  
  @ASTNodeAnnotation.Attribute
  public Declaration localLookup(String label) {
    Object _parameters = label;
    if (localLookup_String_values == null) localLookup_String_values = new java.util.HashMap(4);
    
    
    if (localLookup_String_handler == null) {
      localLookup_String_handler = new ASTNode$DepGraphNode(this, "localLookup_String");
    }
    state().addHandlerDepTo(localLookup_String_handler);
    
    
    
    
    if(localLookup_String_values.containsKey(_parameters)) {
      return (Declaration)localLookup_String_values.get(_parameters);
    }
    
    
    ASTNode$DepGraphNode tmpHandler = new ASTNode$DepGraphNode(this, "");
    state().enterAttrStoreEval(tmpHandler);
    ASTNode$State state = state();
    boolean intermediate = state.INTERMEDIATE_VALUE;
    state.INTERMEDIATE_VALUE = false;
    int num = state.boundariesCrossed;
    boolean isFinal = this.is$Final();
    Declaration localLookup_String_value = localLookup_compute(label);
    if (isFinal && num == state().boundariesCrossed) {
      
      
      localLookup_String_handler.transferDependenciesFrom(tmpHandler);
      localLookup_String_values.put(_parameters, localLookup_String_value);
    } else {
      
      
      tmpHandler.clearDependencies();
    }
    state.INTERMEDIATE_VALUE |= intermediate;

    
    
    state().exitAttrStoreEval(tmpHandler);
    
    
    
    
    return localLookup_String_value;
  }
  /**
   * @apilevel internal
   */
  private Declaration localLookup_compute(String label) {
  	   for(Declaration d : getBodyList())
  		   if (label.matches("-?[0-9]+") || label.equals("*") || label.equals(d.getName()))
  			   return d;
  	   return null;
  	}
  /**
   * @apilevel internal
   */
  protected boolean isCorrect_computed = false;
  /**
   * @apilevel internal
   */
  protected boolean isCorrect_value;
/**
 * @apilevel internal
 */
private void isCorrect_reset() {
  isCorrect_computed = false;
}  
  @ASTNodeAnnotation.Attribute
  public boolean isCorrect() {
    
    
    if (isCorrect_handler == null) {
      isCorrect_handler = new ASTNode$DepGraphNode(this, "isCorrect");
    }
    state().addHandlerDepTo(isCorrect_handler);
    
    
    
    
    if(isCorrect_computed) {
      return isCorrect_value;
    }
    
    
    ASTNode$DepGraphNode tmpHandler = new ASTNode$DepGraphNode(this, "");
    state().enterAttrStoreEval(tmpHandler);
    ASTNode$State state = state();
    boolean intermediate = state.INTERMEDIATE_VALUE;
    state.INTERMEDIATE_VALUE = false;
    int num = state.boundariesCrossed;
    boolean isFinal = this.is$Final();
    isCorrect_value = isCorrect_compute();
    if (isFinal && num == state().boundariesCrossed) {
      
      
      isCorrect_handler.transferDependenciesFrom(tmpHandler);
      isCorrect_computed = true;
    } else {
      
      
      tmpHandler.clearDependencies();
    }
    state.INTERMEDIATE_VALUE |= intermediate;

    
    
    state().exitAttrStoreEval(tmpHandler);
    
    
    
    
    return isCorrect_value;
  }
  /**
   * @apilevel internal
   */
  private boolean isCorrect_compute() {
  		if (!isCorrectLocal())
  			return false;
  		for (Declaration d : getBodyList())
  			if (!d.isCorrect())
  				return false;
  		return true;
  	}
  /**
   * @attribute inh
   * @aspect NameAnalysis
   * @declaredat specifications/NameAnalysis.jrag:119
   */
  @ASTNodeAnnotation.Attribute
  public Declaration lookupRef(String refName) {
    Object _parameters = refName;
    if (lookupRef_String_values == null) lookupRef_String_values = new java.util.HashMap(4);
    
    
    if (lookupRef_String_handler == null) {
      lookupRef_String_handler = new ASTNode$DepGraphNode(this, "lookupRef_String");
    }
    state().addHandlerDepTo(lookupRef_String_handler);
    
    
    
    
    if(lookupRef_String_values.containsKey(_parameters)) {
      return (Declaration)lookupRef_String_values.get(_parameters);
    }
    
    
    ASTNode$DepGraphNode tmpHandler = new ASTNode$DepGraphNode(this, "");
    state().enterAttrStoreEval(tmpHandler);
    ASTNode$State state = state();
    boolean intermediate = state.INTERMEDIATE_VALUE;
    state.INTERMEDIATE_VALUE = false;
    int num = state.boundariesCrossed;
    boolean isFinal = this.is$Final();
    Declaration lookupRef_String_value = getParent().Define_Declaration_lookupRef(this, null, refName);
    if (isFinal && num == state().boundariesCrossed) {
      
      
      lookupRef_String_handler.transferDependenciesFrom(tmpHandler);
      lookupRef_String_values.put(_parameters, lookupRef_String_value);
    } else {
      
      
      tmpHandler.clearDependencies();
    }
    state.INTERMEDIATE_VALUE |= intermediate;

    
    
    state().exitAttrStoreEval(tmpHandler);
    
    
    
    
    return lookupRef_String_value;
  }
  protected java.util.Map lookupRef_String_values;
/**
 * @apilevel internal
 */
private void lookupRef_String_reset() {
  lookupRef_String_values = null;
}  
  /**
   * @declaredat specifications/NameAnalysis.jrag:128
   * @apilevel internal
   */
  public Declaration Define_Declaration_lookupRef(ASTNode caller, ASTNode child, String refName) {
    if (caller == getBodyListNoTransform()) {
      int i = caller.getIndexOfChild(child);
      {
		Declaration match = getBody(i).lookup(refName);
		return (match != null) ? match : lookupRef(refName);
	}
    }
    else {
      return getParent().Define_Declaration_lookupRef(this, caller, refName);
    }
  }
  /**
   * @apilevel internal
   */
  public ASTNode rewriteTo() {    return super.rewriteTo();
  }}
