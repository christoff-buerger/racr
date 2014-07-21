/* This file was generated with JastAdd2 (http://jastadd.org) version 2.1.8 */
package cjava.ast;

import java.util.regex.*;
import java.io.*;
/**
 * This program and the accompanying materials are made available under the
 * terms of the MIT license (X11 license) which accompanies this distribution.
 * 
 * @author M. Tasi\u0192\u00e1, C. B\u221a\u00barger
 * @ast node
 * @declaredat specifications/AST.ast:8
 * @production CompilationUnit : {@link ASTNode} ::= <span class="component">Body:{@link ClassDeclaration}*</span> <span class="component">CompositionProgram:{@link CompositionProgram}</span>;

 */
public class CompilationUnit extends ASTNode<ASTNode> implements Cloneable {
  /**
   * @aspect CompositionsIterative
   * @declaredat specifications/CompositionsIterative.jrag:9
   */
  public void performCompositions() {
		for (BindComposer next = nextComposition(); next != null; next = nextComposition()) {
			cjava.Main.strategy.execute();
			Declaration source = next.Source();
			Declaration target = next.Target();
			if (source == null)
				throw new cjava.CompositionException("Incorrect composition source fragment."); 
			ASTNode parent = target.getParent();
			int index = parent.getIndexOfChild(target);
			parent.setChild(source.treeCopyNoTransform(), index);
		}
	}
  /**
   * @declaredat ASTNode:1
   */
  public CompilationUnit() {
    super();
    is$Final(true);
  }
  /**
   * Initializes the child array to the correct size.
   * Initializes List and Opt nta children.
   * @apilevel internal
   * @ast method
   * @declaredat ASTNode:11
   */
  public void init$Children() {
    children = new ASTNode[2];
    state().enterConstruction();
    setChild(new List(), 0);
    state().exitConstruction();
  }
  /**
   * @declaredat ASTNode:17
   */
  public CompilationUnit(List<ClassDeclaration> p0, CompositionProgram p1) {
state().enterConstruction();
    setChild(p0, 0);
    setChild(p1, 1);
    is$Final(true);
state().exitConstruction();
  }
  /**
   * @apilevel low-level
   * @declaredat ASTNode:27
   */
  protected int numChildren() {
    
    
    state().addHandlerDepTo(numChildren_handler);
    return 2;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:36
   */
  public boolean mayHaveRewrite() {
    return false;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:42
   */
  public void flushAttrCache() {
    super.flushAttrCache();
    nextComposition_reset();
    lookup_String_reset();
    localLookup_String_reset();
    lookupRef_String_reset();
    isCorrect_reset();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:53
   */
  public void flushCollectionCache() {
    super.flushCollectionCache();
  }
  /**
   * @api internal
   * @declaredat ASTNode:59
   */
  public void flushRewriteCache() {
    super.flushRewriteCache();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:65
   */
  public CompilationUnit clone() throws CloneNotSupportedException {
    CompilationUnit node = (CompilationUnit) super.clone();
    return node;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:72
   */
  public CompilationUnit copy() {
    try {
      CompilationUnit node = (CompilationUnit) clone();
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
   * @declaredat ASTNode:98
   */
  public CompilationUnit fullCopy() {
    return treeCopyNoTransform();
  }
  /**
   * Create a deep copy of the AST subtree at this node.
   * The copy is dangling, i.e. has no parent.
   * @return dangling copy of the subtree at this node
   * @apilevel low-level
   * @declaredat ASTNode:107
   */
  public CompilationUnit treeCopyNoTransform() {
    CompilationUnit tree = (CompilationUnit) copy();
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
   * @declaredat ASTNode:128
   */
  public CompilationUnit treeCopy() {
    doFullTraversal();
    return treeCopyNoTransform();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:135
   */
  protected boolean is$Equal(ASTNode node) {
    return super.is$Equal(node) ;    
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:141
   */
  protected boolean childIsNTA(int index) {
    return super.childIsNTA(index);
  }
  /**
   * @declaredat ASTNode:144
   */
  protected ASTNode$DepGraphNode nextComposition_handler;
  /**
   * @declaredat ASTNode:145
   */
  protected ASTNode$DepGraphNode lookup_String_handler;
  /**
   * @declaredat ASTNode:146
   */
  protected ASTNode$DepGraphNode localLookup_String_handler;
  /**
   * @declaredat ASTNode:147
   */
  protected ASTNode$DepGraphNode lookupRef_String_handler;
  /**
   * @declaredat ASTNode:148
   */
  protected ASTNode$DepGraphNode isCorrect_handler;
  /**
   * @declaredat ASTNode:149
   */
  protected void inc_copyHandlers(CompilationUnit copy) {
    super.inc_copyHandlers(copy);

        if (nextComposition_handler != null) {
          copy.nextComposition_handler = new ASTNode$DepGraphNode(nextComposition_handler, copy);
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
        if (isCorrect_handler != null) {
          copy.isCorrect_handler = new ASTNode$DepGraphNode(isCorrect_handler, copy);
        }
  }
  /**
   * @api internal
   * @declaredat ASTNode:173
   */
  public void reactToDependencyChange(String attrID) {
    
    // flush children
    
    
    
    
    
    if (attrID.equals("nextComposition") && nextComposition_computed) {
      nextComposition_computed = false;
      nextComposition_value = null;
      nextComposition_handler.notifyDependencies();
      super.reactToDependencyChange(attrID);
      return;
    }
    else 
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
    if (attrID.equals("lookupRef_String") && lookupRef_String_values != null && !lookupRef_String_values.isEmpty()) {
      lookupRef_String_values = null;
      lookupRef_String_handler.notifyDependencies();
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
    super.reactToDependencyChange(attrID);
  }
  /**
   * @api internal
   * @declaredat ASTNode:227
   */
  protected void inc_flush_subtree(ASTNode$DepGraphNode h) {
    inc_state = inc_GARBAGE;
    h.transferSetsFrom(nextComposition_handler);
    
    h.transferSetsFrom(lookup_String_handler);
    
    h.transferSetsFrom(localLookup_String_handler);
    
    h.transferSetsFrom(lookupRef_String_handler);
    
    h.transferSetsFrom(isCorrect_handler);
    super.inc_flush_subtree(h);
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:246
   */
  public void inc_changeState(int newState) {
  super.inc_changeState(newState);
  if (nextComposition_handler != null) {
    nextComposition_handler.changeState(newState);
  }
  if (lookup_String_handler != null) {
    lookup_String_handler.changeState(newState);
  }
  if (localLookup_String_handler != null) {
    localLookup_String_handler.changeState(newState);
  }
  if (lookupRef_String_handler != null) {
    lookupRef_String_handler.changeState(newState);
  }
  if (isCorrect_handler != null) {
    isCorrect_handler.changeState(newState);
  }
}
  /**
   * @apilevel internal
   * @declaredat ASTNode:267
   */
  public void inc_throwAway() {
  inc_state = inc_GARBAGE;
  super.inc_throwAway();
  if (nextComposition_handler != null) {
    nextComposition_handler.throwAway();
  }
  if (lookup_String_handler != null) {
    lookup_String_handler.throwAway();
  }
  if (localLookup_String_handler != null) {
    localLookup_String_handler.throwAway();
  }
  if (lookupRef_String_handler != null) {
    lookupRef_String_handler.throwAway();
  }
  if (isCorrect_handler != null) {
    isCorrect_handler.throwAway();
  }
}
  /**
   * Replaces the Body list.
   * @param list The new list node to be used as the Body list.
   * @apilevel high-level
   */
  public void setBodyList(List<ClassDeclaration> list) {
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
  public ClassDeclaration getBody(int i) {
    return (ClassDeclaration) getBodyList().getChild(i);
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
  public void addBody(ClassDeclaration node) {
    List<ClassDeclaration> list = (parent == null || state == null) ? getBodyListNoTransform() : getBodyList();
    list.addChild(node);
  }
  /**
   * @apilevel low-level
   */
  public void addBodyNoTransform(ClassDeclaration node) {
    List<ClassDeclaration> list = getBodyListNoTransform();
    list.addChild(node);
  }
  /**
   * Replaces the Body list element at index {@code i} with the new node {@code node}.
   * @param node The new node to replace the old list element.
   * @param i The list index of the node to be replaced.
   * @apilevel high-level
   */
  public void setBody(ClassDeclaration node, int i) {
    List<ClassDeclaration> list = getBodyList();
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
  public List<ClassDeclaration> getBodyList() {
    List<ClassDeclaration> list = (List<ClassDeclaration>) getChild(0);
    list.getNumChild();
    return list;
  }
  /**
   * Retrieves the Body list.
   * <p><em>This method does not invoke AST transformations.</em></p>
   * @return The node representing the Body list.
   * @apilevel low-level
   */
  public List<ClassDeclaration> getBodyListNoTransform() {
    return (List<ClassDeclaration>) getChildNoTransform(0);
  }
  /**
   * Retrieves the Body list.
   * @return The node representing the Body list.
   * @apilevel high-level
   */
  public List<ClassDeclaration> getBodys() {
    return getBodyList();
  }
  /**
   * Retrieves the Body list.
   * <p><em>This method does not invoke AST transformations.</em></p>
   * @return The node representing the Body list.
   * @apilevel low-level
   */
  public List<ClassDeclaration> getBodysNoTransform() {
    return getBodyListNoTransform();
  }
  /**
   * Replaces the CompositionProgram child.
   * @param node The new node to replace the CompositionProgram child.
   * @apilevel high-level
   */
  public void setCompositionProgram(CompositionProgram node) {
    state().enterConstruction();
    setChild(node, 1);
    state().exitConstruction();
  }
  /**
   * Retrieves the CompositionProgram child.
   * @return The current node used as the CompositionProgram child.
   * @apilevel high-level
   */
  @ASTNodeAnnotation.Child(name="CompositionProgram")
  public CompositionProgram getCompositionProgram() {
    return (CompositionProgram) getChild(1);
  }
  /**
   * Retrieves the CompositionProgram child.
   * <p><em>This method does not invoke AST transformations.</em></p>
   * @return The current node used as the CompositionProgram child.
   * @apilevel low-level
   */
  public CompositionProgram getCompositionProgramNoTransform() {
    return (CompositionProgram) getChildNoTransform(1);
  }
  /**
   * @apilevel internal
   */
  protected boolean nextComposition_computed = false;
  /**
   * @apilevel internal
   */
  protected BindComposer nextComposition_value;
/**
 * @apilevel internal
 */
private void nextComposition_reset() {
  nextComposition_computed = false;
  nextComposition_value = null;
}  
  @ASTNodeAnnotation.Attribute
  public BindComposer nextComposition() {
    
    
    if (nextComposition_handler == null) {
      nextComposition_handler = new ASTNode$DepGraphNode(this, "nextComposition");
    }
    state().addHandlerDepTo(nextComposition_handler);
    
    
    
    
    if(nextComposition_computed) {
      return nextComposition_value;
    }
    
    
    ASTNode$DepGraphNode tmpHandler = new ASTNode$DepGraphNode(this, "");
    state().enterAttrStoreEval(tmpHandler);
    ASTNode$State state = state();
    boolean intermediate = state.INTERMEDIATE_VALUE;
    state.INTERMEDIATE_VALUE = false;
    int num = state.boundariesCrossed;
    boolean isFinal = this.is$Final();
    nextComposition_value = getCompositionProgram().nextComposition();
    if (isFinal && num == state().boundariesCrossed) {
      
      
      nextComposition_handler.transferDependenciesFrom(tmpHandler);
      nextComposition_computed = true;
    } else {
      
      
      tmpHandler.clearDependencies();
    }
    state.INTERMEDIATE_VALUE |= intermediate;

    
    
    state().exitAttrStoreEval(tmpHandler);
    
    
    
    
    return nextComposition_value;
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
  		for (Declaration d : getBodyList())
  			if (label.matches("-?[0-9]+") || label.equals("*") || label.equals(d.getName()))
  			   return d;
  		return null;
  	}
  protected java.util.Map lookupRef_String_values;
/**
 * @apilevel internal
 */
private void lookupRef_String_reset() {
  lookupRef_String_values = null;
}  
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
    Declaration lookupRef_String_value = lookup(refName);
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
  		for (Declaration d : getBodyList())
  			if (!d.isCorrect())
  				return false;
  		return true;
  	}
  /**
   * @declaredat specifications/NameAnalysis.jrag:123
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
   * @declaredat specifications/SupportApi.jrag:11
   * @apilevel internal
   */
  public CompilationUnit Define_CompilationUnit_CompilationUnit(ASTNode caller, ASTNode child) {
    if (caller == getCompositionProgramNoTransform()) {
      return this;
    }
    else {
      return getParent().Define_CompilationUnit_CompilationUnit(this, caller);
    }
  }
  /**
   * @apilevel internal
   */
  public ASTNode rewriteTo() {    return super.rewriteTo();
  }}
