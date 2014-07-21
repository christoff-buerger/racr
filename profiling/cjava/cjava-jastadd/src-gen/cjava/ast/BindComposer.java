/* This file was generated with JastAdd2 (http://jastadd.org) version 2.1.8 */
package cjava.ast;

import java.util.regex.*;
import java.io.*;
/**
 * @ast node
 * @declaredat specifications/AST.ast:15
 * @production BindComposer : {@link ASTNode} ::= <span class="component">&lt;TargetName:String&gt;</span> <span class="component">&lt;SourceName:String&gt;</span>;

 */
public class BindComposer extends ASTNode<ASTNode> implements Cloneable {
  /**
   * @aspect SupportApi
   * @declaredat specifications/SupportApi.jrag:16
   */
  private String prepareName(String name) {
		String[] parts = name.split("\\.");
		StringBuffer newname = new StringBuffer();
		for (int i = 0; i < parts.length; i++) {
			if (parts[i].equals("**"))
				newname.append("-1");
			else if (parts[i].matches("\\*[0-9]+"))
				newname.append(parts[i].substring(1));
			else newname.append(parts[i]);
			if (i != parts.length - 1)
				newname.append(".");
		}
		return newname.toString();
	}
  /**
   * @declaredat ASTNode:1
   */
  public BindComposer() {
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
    state().enterConstruction();
    state().exitConstruction();
  }
  /**
   * @declaredat ASTNode:14
   */
  public BindComposer(String p0, String p1) {
state().enterConstruction();
    setTargetName(p0);
    setSourceName(p1);
state().exitConstruction();
  }
  /**
   * @apilevel low-level
   * @declaredat ASTNode:23
   */
  protected int numChildren() {
    
    
    state().addHandlerDepTo(numChildren_handler);
    return 0;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:32
   */
  public boolean mayHaveRewrite() {
    return false;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:38
   */
  public void flushAttrCache() {
    super.flushAttrCache();
    isExhausted_reset();
    Source_reset();
    Target_reset();
    sourceName_reset();
    targetName_reset();
    CompilationUnit_reset();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:50
   */
  public void flushCollectionCache() {
    super.flushCollectionCache();
  }
  /**
   * @api internal
   * @declaredat ASTNode:56
   */
  public void flushRewriteCache() {
    super.flushRewriteCache();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:62
   */
  public BindComposer clone() throws CloneNotSupportedException {
    BindComposer node = (BindComposer) super.clone();
    return node;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:69
   */
  public BindComposer copy() {
    try {
      BindComposer node = (BindComposer) clone();
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
   * @declaredat ASTNode:95
   */
  public BindComposer fullCopy() {
    return treeCopyNoTransform();
  }
  /**
   * Create a deep copy of the AST subtree at this node.
   * The copy is dangling, i.e. has no parent.
   * @return dangling copy of the subtree at this node
   * @apilevel low-level
   * @declaredat ASTNode:104
   */
  public BindComposer treeCopyNoTransform() {
    BindComposer tree = (BindComposer) copy();
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
   * @declaredat ASTNode:125
   */
  public BindComposer treeCopy() {
    doFullTraversal();
    return treeCopyNoTransform();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:132
   */
  protected boolean is$Equal(ASTNode node) {
    return super.is$Equal(node)  && (tokenString_TargetName == ((BindComposer)node).tokenString_TargetName) && (tokenString_SourceName == ((BindComposer)node).tokenString_SourceName);    
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:138
   */
  protected boolean childIsNTA(int index) {
    return super.childIsNTA(index);
  }
  /**
   * @declaredat ASTNode:141
   */
  protected ASTNode$DepGraphNode isExhausted_handler;
  /**
   * @declaredat ASTNode:142
   */
  protected ASTNode$DepGraphNode Source_handler;
  /**
   * @declaredat ASTNode:143
   */
  protected ASTNode$DepGraphNode Target_handler;
  /**
   * @declaredat ASTNode:144
   */
  protected ASTNode$DepGraphNode sourceName_handler;
  /**
   * @declaredat ASTNode:145
   */
  protected ASTNode$DepGraphNode targetName_handler;
  /**
   * @declaredat ASTNode:146
   */
  protected ASTNode$DepGraphNode CompilationUnit_handler;
  /**
   * @declaredat ASTNode:147
   */
  protected void inc_copyHandlers(BindComposer copy) {
    super.inc_copyHandlers(copy);

        if (getTargetName_handler != null) {
          copy.getTargetName_handler = new ASTNode$DepGraphNode(getTargetName_handler, copy);
        }
        if (getSourceName_handler != null) {
          copy.getSourceName_handler = new ASTNode$DepGraphNode(getSourceName_handler, copy);
        }
        if (isExhausted_handler != null) {
          copy.isExhausted_handler = new ASTNode$DepGraphNode(isExhausted_handler, copy);
        }
        if (Source_handler != null) {
          copy.Source_handler = new ASTNode$DepGraphNode(Source_handler, copy);
        }
        if (Target_handler != null) {
          copy.Target_handler = new ASTNode$DepGraphNode(Target_handler, copy);
        }
        if (sourceName_handler != null) {
          copy.sourceName_handler = new ASTNode$DepGraphNode(sourceName_handler, copy);
        }
        if (targetName_handler != null) {
          copy.targetName_handler = new ASTNode$DepGraphNode(targetName_handler, copy);
        }
        if (CompilationUnit_handler != null) {
          copy.CompilationUnit_handler = new ASTNode$DepGraphNode(CompilationUnit_handler, copy);
        }
  }
  /**
   * @api internal
   * @declaredat ASTNode:180
   */
  public void reactToDependencyChange(String attrID) {
    
    // flush children
    
    
    
    
    
    if (attrID.equals("isExhausted") && isExhausted_computed) {
      isExhausted_computed = false;
      isExhausted_handler.notifyDependencies();
      super.reactToDependencyChange(attrID);
      return;
    }
    else 
    if (attrID.equals("Source") && Source_computed) {
      Source_computed = false;
      Source_value = null;
      Source_handler.notifyDependencies();
      super.reactToDependencyChange(attrID);
      return;
    }
    else 
    if (attrID.equals("Target") && Target_computed) {
      Target_computed = false;
      Target_value = null;
      Target_handler.notifyDependencies();
      super.reactToDependencyChange(attrID);
      return;
    }
    else 
    if (attrID.equals("sourceName") && sourceName_computed) {
      sourceName_computed = false;
      sourceName_value = null;
      sourceName_handler.notifyDependencies();
      super.reactToDependencyChange(attrID);
      return;
    }
    else 
    if (attrID.equals("targetName") && targetName_computed) {
      targetName_computed = false;
      targetName_value = null;
      targetName_handler.notifyDependencies();
      super.reactToDependencyChange(attrID);
      return;
    }
    else 
    if (attrID.equals("CompilationUnit") && CompilationUnit_computed) {
      CompilationUnit_computed = false;
      CompilationUnit_value = null;
      CompilationUnit_handler.notifyDependencies();
      super.reactToDependencyChange(attrID);
      return;
    }
    super.reactToDependencyChange(attrID);
  }
  /**
   * @api internal
   * @declaredat ASTNode:245
   */
  protected void inc_flush_subtree(ASTNode$DepGraphNode h) {
    inc_state = inc_GARBAGE;
    h.transferSetsFrom(isExhausted_handler);
    
    h.transferSetsFrom(Source_handler);
    
    h.transferSetsFrom(Target_handler);
    
    h.transferSetsFrom(sourceName_handler);
    
    h.transferSetsFrom(targetName_handler);
    
    h.transferSetsFrom(CompilationUnit_handler);
    
    h.transferSetsFrom(getTargetName_handler);
    
    h.transferSetsFrom(getSourceName_handler);
    super.inc_flush_subtree(h);
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:270
   */
  public void inc_changeState(int newState) {
  super.inc_changeState(newState);
  if (getTargetName_handler != null) {
    getTargetName_handler.changeState(newState);
  }
  if (getSourceName_handler != null) {
    getSourceName_handler.changeState(newState);
  }
  if (isExhausted_handler != null) {
    isExhausted_handler.changeState(newState);
  }
  if (Source_handler != null) {
    Source_handler.changeState(newState);
  }
  if (Target_handler != null) {
    Target_handler.changeState(newState);
  }
  if (sourceName_handler != null) {
    sourceName_handler.changeState(newState);
  }
  if (targetName_handler != null) {
    targetName_handler.changeState(newState);
  }
  if (CompilationUnit_handler != null) {
    CompilationUnit_handler.changeState(newState);
  }
}
  /**
   * @apilevel internal
   * @declaredat ASTNode:300
   */
  public void inc_throwAway() {
  inc_state = inc_GARBAGE;
  super.inc_throwAway();
  if (getTargetName_handler != null) {
    getTargetName_handler.throwAway();
  }
  if (getSourceName_handler != null) {
    getSourceName_handler.throwAway();
  }
  if (isExhausted_handler != null) {
    isExhausted_handler.throwAway();
  }
  if (Source_handler != null) {
    Source_handler.throwAway();
  }
  if (Target_handler != null) {
    Target_handler.throwAway();
  }
  if (sourceName_handler != null) {
    sourceName_handler.throwAway();
  }
  if (targetName_handler != null) {
    targetName_handler.throwAway();
  }
  if (CompilationUnit_handler != null) {
    CompilationUnit_handler.throwAway();
  }
}
  /**
   */
  protected ASTNode$DepGraphNode getTargetName_handler = new ASTNode$DepGraphNode(this, "getTargetName");
  /**
   * Replaces the lexeme TargetName.
   * @param value The new value for the lexeme TargetName.
   * @apilevel high-level
   */
  public void setTargetName(String value) {
    tokenString_TargetName = value;
    
    
    if (!state().IN_CONSTRUCTION && !state().IN_ATTR_STORE_EVAL) {
      getTargetName_handler.notifyDependencies();
    
    
    
      ASTNode initial = inc_locateInitialCopy();
      if (initial != null) {
        state().enterConstruction();
        ((BindComposer)initial).setTargetName(tokenString_TargetName);
        state().exitConstruction();
        return;
      }
    
    }
  }
  /**
   * @apilevel internal
   */
  protected String tokenString_TargetName;
  /**
   * Retrieves the value for the lexeme TargetName.
   * @return The value for the lexeme TargetName.
   * @apilevel high-level
   */
  @ASTNodeAnnotation.Token(name="TargetName")
  public String getTargetName() {
    
    
    state().addHandlerDepTo(getTargetName_handler);
    return tokenString_TargetName != null ? tokenString_TargetName : "";
  }
  /**
   */
  protected ASTNode$DepGraphNode getSourceName_handler = new ASTNode$DepGraphNode(this, "getSourceName");
  /**
   * Replaces the lexeme SourceName.
   * @param value The new value for the lexeme SourceName.
   * @apilevel high-level
   */
  public void setSourceName(String value) {
    tokenString_SourceName = value;
    
    
    if (!state().IN_CONSTRUCTION && !state().IN_ATTR_STORE_EVAL) {
      getSourceName_handler.notifyDependencies();
    
    
    
      ASTNode initial = inc_locateInitialCopy();
      if (initial != null) {
        state().enterConstruction();
        ((BindComposer)initial).setSourceName(tokenString_SourceName);
        state().exitConstruction();
        return;
      }
    
    }
  }
  /**
   * @apilevel internal
   */
  protected String tokenString_SourceName;
  /**
   * Retrieves the value for the lexeme SourceName.
   * @return The value for the lexeme SourceName.
   * @apilevel high-level
   */
  @ASTNodeAnnotation.Token(name="SourceName")
  public String getSourceName() {
    
    
    state().addHandlerDepTo(getSourceName_handler);
    return tokenString_SourceName != null ? tokenString_SourceName : "";
  }
  /**
   * @apilevel internal
   */
  protected boolean isExhausted_computed = false;
  /**
   * @apilevel internal
   */
  protected boolean isExhausted_value;
/**
 * @apilevel internal
 */
private void isExhausted_reset() {
  isExhausted_computed = false;
}  
  @ASTNodeAnnotation.Attribute
  public boolean isExhausted() {
    
    
    if (isExhausted_handler == null) {
      isExhausted_handler = new ASTNode$DepGraphNode(this, "isExhausted");
    }
    state().addHandlerDepTo(isExhausted_handler);
    
    
    
    
    if(isExhausted_computed) {
      return isExhausted_value;
    }
    
    
    ASTNode$DepGraphNode tmpHandler = new ASTNode$DepGraphNode(this, "");
    state().enterAttrStoreEval(tmpHandler);
    ASTNode$State state = state();
    boolean intermediate = state.INTERMEDIATE_VALUE;
    state.INTERMEDIATE_VALUE = false;
    int num = state.boundariesCrossed;
    boolean isFinal = this.is$Final();
    isExhausted_value = Target() == null;
    if (isFinal && num == state().boundariesCrossed) {
      
      
      isExhausted_handler.transferDependenciesFrom(tmpHandler);
      isExhausted_computed = true;
    } else {
      
      
      tmpHandler.clearDependencies();
    }
    state.INTERMEDIATE_VALUE |= intermediate;

    
    
    state().exitAttrStoreEval(tmpHandler);
    
    
    
    
    return isExhausted_value;
  }
  /**
   * @apilevel internal
   */
  protected boolean Source_computed = false;
  /**
   * @apilevel internal
   */
  protected Declaration Source_value;
/**
 * @apilevel internal
 */
private void Source_reset() {
  Source_computed = false;
  Source_value = null;
}  
  @ASTNodeAnnotation.Attribute
  public Declaration Source() {
    
    
    if (Source_handler == null) {
      Source_handler = new ASTNode$DepGraphNode(this, "Source");
    }
    state().addHandlerDepTo(Source_handler);
    
    
    
    
    if(Source_computed) {
      return Source_value;
    }
    
    
    ASTNode$DepGraphNode tmpHandler = new ASTNode$DepGraphNode(this, "");
    state().enterAttrStoreEval(tmpHandler);
    ASTNode$State state = state();
    boolean intermediate = state.INTERMEDIATE_VALUE;
    state.INTERMEDIATE_VALUE = false;
    int num = state.boundariesCrossed;
    boolean isFinal = this.is$Final();
    Source_value = CompilationUnit().lookup(sourceName());
    if (isFinal && num == state().boundariesCrossed) {
      
      
      Source_handler.transferDependenciesFrom(tmpHandler);
      Source_computed = true;
    } else {
      
      
      tmpHandler.clearDependencies();
    }
    state.INTERMEDIATE_VALUE |= intermediate;

    
    
    state().exitAttrStoreEval(tmpHandler);
    
    
    
    
    return Source_value;
  }
  /**
   * @apilevel internal
   */
  protected boolean Target_computed = false;
  /**
   * @apilevel internal
   */
  protected Declaration Target_value;
/**
 * @apilevel internal
 */
private void Target_reset() {
  Target_computed = false;
  Target_value = null;
}  
  @ASTNodeAnnotation.Attribute
  public Declaration Target() {
    
    
    if (Target_handler == null) {
      Target_handler = new ASTNode$DepGraphNode(this, "Target");
    }
    state().addHandlerDepTo(Target_handler);
    
    
    
    
    if(Target_computed) {
      return Target_value;
    }
    
    
    ASTNode$DepGraphNode tmpHandler = new ASTNode$DepGraphNode(this, "");
    state().enterAttrStoreEval(tmpHandler);
    ASTNode$State state = state();
    boolean intermediate = state.INTERMEDIATE_VALUE;
    state.INTERMEDIATE_VALUE = false;
    int num = state.boundariesCrossed;
    boolean isFinal = this.is$Final();
    Target_value = CompilationUnit().lookup(targetName());
    if (isFinal && num == state().boundariesCrossed) {
      
      
      Target_handler.transferDependenciesFrom(tmpHandler);
      Target_computed = true;
    } else {
      
      
      tmpHandler.clearDependencies();
    }
    state.INTERMEDIATE_VALUE |= intermediate;

    
    
    state().exitAttrStoreEval(tmpHandler);
    
    
    
    
    return Target_value;
  }
  /**
   * @apilevel internal
   */
  protected boolean sourceName_computed = false;
  /**
   * @apilevel internal
   */
  protected String sourceName_value;
/**
 * @apilevel internal
 */
private void sourceName_reset() {
  sourceName_computed = false;
  sourceName_value = null;
}  
  @ASTNodeAnnotation.Attribute
  public String sourceName() {
    
    
    if (sourceName_handler == null) {
      sourceName_handler = new ASTNode$DepGraphNode(this, "sourceName");
    }
    state().addHandlerDepTo(sourceName_handler);
    
    
    
    
    if(sourceName_computed) {
      return sourceName_value;
    }
    
    
    ASTNode$DepGraphNode tmpHandler = new ASTNode$DepGraphNode(this, "");
    state().enterAttrStoreEval(tmpHandler);
    ASTNode$State state = state();
    boolean intermediate = state.INTERMEDIATE_VALUE;
    state.INTERMEDIATE_VALUE = false;
    int num = state.boundariesCrossed;
    boolean isFinal = this.is$Final();
    sourceName_value = prepareName(getSourceName());
    if (isFinal && num == state().boundariesCrossed) {
      
      
      sourceName_handler.transferDependenciesFrom(tmpHandler);
      sourceName_computed = true;
    } else {
      
      
      tmpHandler.clearDependencies();
    }
    state.INTERMEDIATE_VALUE |= intermediate;

    
    
    state().exitAttrStoreEval(tmpHandler);
    
    
    
    
    return sourceName_value;
  }
  /**
   * @apilevel internal
   */
  protected boolean targetName_computed = false;
  /**
   * @apilevel internal
   */
  protected String targetName_value;
/**
 * @apilevel internal
 */
private void targetName_reset() {
  targetName_computed = false;
  targetName_value = null;
}  
  @ASTNodeAnnotation.Attribute
  public String targetName() {
    
    
    if (targetName_handler == null) {
      targetName_handler = new ASTNode$DepGraphNode(this, "targetName");
    }
    state().addHandlerDepTo(targetName_handler);
    
    
    
    
    if(targetName_computed) {
      return targetName_value;
    }
    
    
    ASTNode$DepGraphNode tmpHandler = new ASTNode$DepGraphNode(this, "");
    state().enterAttrStoreEval(tmpHandler);
    ASTNode$State state = state();
    boolean intermediate = state.INTERMEDIATE_VALUE;
    state.INTERMEDIATE_VALUE = false;
    int num = state.boundariesCrossed;
    boolean isFinal = this.is$Final();
    targetName_value = prepareName(getTargetName());
    if (isFinal && num == state().boundariesCrossed) {
      
      
      targetName_handler.transferDependenciesFrom(tmpHandler);
      targetName_computed = true;
    } else {
      
      
      tmpHandler.clearDependencies();
    }
    state.INTERMEDIATE_VALUE |= intermediate;

    
    
    state().exitAttrStoreEval(tmpHandler);
    
    
    
    
    return targetName_value;
  }
  /**
   * @attribute inh
   * @aspect SupportApi
   * @declaredat specifications/SupportApi.jrag:10
   */
  @ASTNodeAnnotation.Attribute
  public CompilationUnit CompilationUnit() {
    
    
    if (CompilationUnit_handler == null) {
      CompilationUnit_handler = new ASTNode$DepGraphNode(this, "CompilationUnit");
    }
    state().addHandlerDepTo(CompilationUnit_handler);
    
    
    
    
    if(CompilationUnit_computed) {
      return CompilationUnit_value;
    }
    
    
    ASTNode$DepGraphNode tmpHandler = new ASTNode$DepGraphNode(this, "");
    state().enterAttrStoreEval(tmpHandler);
    ASTNode$State state = state();
    boolean intermediate = state.INTERMEDIATE_VALUE;
    state.INTERMEDIATE_VALUE = false;
    int num = state.boundariesCrossed;
    boolean isFinal = this.is$Final();
    CompilationUnit_value = getParent().Define_CompilationUnit_CompilationUnit(this, null);
    if (isFinal && num == state().boundariesCrossed) {
      
      
      CompilationUnit_handler.transferDependenciesFrom(tmpHandler);
      CompilationUnit_computed = true;
    } else {
      
      
      tmpHandler.clearDependencies();
    }
    state.INTERMEDIATE_VALUE |= intermediate;

    
    
    state().exitAttrStoreEval(tmpHandler);
    
    
    
    
    return CompilationUnit_value;
  }
  /**
   * @apilevel internal
   */
  protected boolean CompilationUnit_computed = false;
  /**
   * @apilevel internal
   */
  protected CompilationUnit CompilationUnit_value;
/**
 * @apilevel internal
 */
private void CompilationUnit_reset() {
  CompilationUnit_computed = false;
  CompilationUnit_value = null;
}  
  /**
   * @apilevel internal
   */
  public ASTNode rewriteTo() {    return super.rewriteTo();
  }}
