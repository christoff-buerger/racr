/* This file was generated with JastAdd2 (http://jastadd.org) version 2.1.8 */
package cjava.ast;

import java.util.regex.*;
import java.io.*;
/**
 * @ast node
 * @production ASTNode;

 */
public class ASTNode<T extends ASTNode> implements Cloneable, Iterable<T> {
  /**
   * @aspect NameAnalysis
   * @declaredat specifications/NameAnalysis.jrag:14
   */
  private static Declaration subtreeLookup(ASTNode decl, String label) {
		Declaration match = null;
		if (decl instanceof ClassDeclaration) {
			for (Declaration d : ((ClassDeclaration)decl).getBodyList()) {
				match = d.lookup(label);
				if (match != null)
					return match;
			}
		} else {
			for (Declaration d : ((CompilationUnit)decl).getBodyList()) {
				match = d.lookup(label);
				if (match != null)
					return match;
			}
		}
		return null;
	}
  /**
   * @aspect NameAnalysis
   * @declaredat specifications/NameAnalysis.jrag:32
   */
  public static Declaration doLookup(ASTNode decl, String label) {
		String[] parts = new String[2];
		int dotindex = label.indexOf('.');
		if (dotindex == -1) {
			parts[0] = label;
			parts[1] = "";
		} else {
			parts[0] = label.substring(0, dotindex);
			parts[1] = label.substring(dotindex + 1);
		}
		
		Declaration localMatch = decl.localLookup(parts[0]);
		if (parts[1].equals(""))
			return localMatch;
		if (localMatch != null) {
			if (parts[0].equals("-1")) {
				Declaration match = decl.lookup(parts[1]);
				if (match != null)
					return match;
				return subtreeLookup(decl, label);
			}
			if (parts[0].equals("0"))
				return decl.lookup(parts[1]);
			if (parts[0].matches("[1-9][0-9]*")) {
				Declaration match = decl.lookup(parts[1]);
				if (match != null)
					return match;
				int num = Integer.parseInt(parts[0]) - 1;
				return subtreeLookup(decl, "" + num + "." + parts[1]);
			}
			return localMatch.lookup(parts[1]);
		}
		return null;
	}
  /**
   * @declaredat ASTNode:1
   */
  public ASTNode() {
    super();
    init$Children();
  }
  /**
   * Initializes the child array to the correct size.
   * Initializes List and Opt nta children.
   * @apilevel internal
   * @ast method
   * @declaredat ASTNode:11
   */
  public void init$Children() {
    state().enterConstruction();
    state().exitConstruction();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:18
   */
  private int childIndex;
  /**
   * @apilevel low-level
   * @declaredat ASTNode:23
   */
  public int getIndexOfChild(ASTNode node) {
    if (node == null) {
      return -1;
    }
    if (node.childIndex < numChildren && node == children[node.childIndex]) {
      return node.childIndex;
    }
    for(int i = 0; children != null && i < children.length; i++) {
      if(children[i] == node) {
        node.childIndex = i;
        return i;
      }
    }
    return -1;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:42
   */
  public static final boolean generatedWithCircularEnabled = true;
  /**
   * @apilevel internal
   * @declaredat ASTNode:46
   */
  public static final boolean generatedWithCacheCycle = true;
  /**
   * @apilevel internal
   * @declaredat ASTNode:50
   */
  public static final boolean generatedWithComponentCheck = false;
  /**
   * Parent pointer
   * @apilevel low-level
   * @declaredat ASTNode:56
   */
  protected ASTNode parent;
  /**
   * Child array
   * @apilevel low-level
   * @declaredat ASTNode:62
   */
  protected ASTNode[] children;
  /**
   * @apilevel internal
   * @declaredat ASTNode:68
   */
  protected static ASTNode$State state = new ASTNode$State();
  /**
   * @apilevel internal
   * @declaredat ASTNode:73
   */
  public final ASTNode$State state() {
    return state;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:80
   */
  public boolean in$Circle = false;
  /**
   * @apilevel internal
   * @declaredat ASTNode:85
   */
  public boolean in$Circle() {
    return in$Circle;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:92
   */
  public void in$Circle(boolean b) {
    in$Circle = b;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:99
   */
  public boolean is$Final = false;
  /**
   * @apilevel internal
   * @declaredat ASTNode:103
   */
  public boolean is$Final() { return is$Final; }
  /**
   * @apilevel internal
   * @declaredat ASTNode:107
   */
  public void is$Final(boolean b) { is$Final = b; }
  /**
   * @apilevel low-level
   * @declaredat ASTNode:112
   */
  public T getChild(int i) {

    ASTNode node = this.getChildNoTransform(i);
    if(node == null) {
      return null;
    }
    if(node.is$Final()) {
      
      
      this.state().addHandlerDepTo(this.getChild_handler);
      return (T) node;
    }
    if(!node.mayHaveRewrite()) {
      node.is$Final(this.is$Final());
      
      
      this.state().addHandlerDepTo(this.getChild_handler);
      return (T) node;
    }
    if(!node.in$Circle()) {
      if (!node.getChild_hasEnclosingInitValue()) {
        if(this.init_children == null) {
          this.init_children = new ASTNode[this.children.length];
          this.children_computed = new boolean[this.children.length];
        }
        this.init_children[i] = node.treeCopyNoTransform();
        this.children_computed[i] = true;
      }
      
      
      
      this.state().enterRewriteEval(this.getChild_handler);
      int rewriteState;
      int num = this.state().boundariesCrossed;
      do {
        this.state().push(ASTNode$State.REWRITE_CHANGE);
        ASTNode oldNode = node;
        oldNode.in$Circle(true);
        node = node.rewriteTo();
        if(node != oldNode) {
          
          //ASTNode initial = this.init_children[i];
          this.setChild(node, i);
          
          
          oldNode.inc_flush_subtree(this.getChild_handler);
          //this.init_children[i] = initial;
        }
        oldNode.in$Circle(false);
        rewriteState = this.state().pop();
      } while(rewriteState == ASTNode$State.REWRITE_CHANGE);
      if(rewriteState == ASTNode$State.REWRITE_NOCHANGE && this.is$Final()) {
        node.is$Final(true);
        this.state().boundariesCrossed = num;
      } else {
      }
      
      
      this.state().exitRewriteEval(this.getChild_handler);
    } else if(this.is$Final() != node.is$Final()) {
      this.state().boundariesCrossed++;
    } else {
    }
    
    
    this.state().addHandlerDepTo(this.getChild_handler);
    return (T) node;


  }
  /**
   * @apilevel low-level
   * @declaredat ASTNode:185
   */
  public void addChild(T node) {
    setChild(node, getNumChildNoTransform());
    
    
    state().addHandlerDepTo(getChild_handler);
    if (state().IN_ATTR_STORE_EVAL && !node.mayHaveRewrite()) {
      node.is$Final(is$Final());
    }
  }
  /**
   * <p><em>This method does not invoke AST transformations.</em></p>
   * @apilevel low-level
   * @declaredat ASTNode:198
   */
  public T getChildNoTransform(int i) {
  // Must be able to override get child methods for incremental evaluation
    if (children == null) {
      return null;
    }
    T child = (T)children[i];
    
    
    state().addHandlerDepTo(getChild_handler);
    return child;
  }
  /**
   * @apilevel low-level
   * @declaredat ASTNode:212
   */
  protected int numChildren;
  /**
   * @apilevel low-level
   * @declaredat ASTNode:217
   */
  protected int numChildren() {
    
    
    state().addHandlerDepTo(numChildren_handler);
    return numChildren;
  }
  /**
   * @apilevel low-level
   * @declaredat ASTNode:227
   */
  public int getNumChild() {
    return numChildren();
  }
  /**
   * <p><em>This method does not invoke AST transformations.</em></p>
   * @apilevel low-level
   * @declaredat ASTNode:235
   */
  public final int getNumChildNoTransform() {
    return numChildren();
  }
  /**
   * @apilevel low-level
   * @declaredat ASTNode:241
   */
  public void setChild(ASTNode node, int i) {
    
    
    if (!state().IN_CONSTRUCTION && !state().IN_ATTR_STORE_EVAL) {
      if (children != null && i < children.length && children[i] != null) {
        children[i].inc_notifyForRemove();
      }
      getChild_handler.flushRegion();
    
    
    
      ASTNode initial  = inc_locateInitialCopy();
      if (initial != null) {
        state().enterConstruction();
        if (i >= initial.numChildren) {
          initial.addChild(node);
        } else {
          initial.setChild(node, i);
        }
        state().exitConstruction();
        return;
      }
    
    }
    if(children == null) {
      children = new ASTNode[(i+1>4 || !(this instanceof List))?i+1:4];
    } else if (i >= children.length) {
      ASTNode c[] = new ASTNode[i << 1];
      System.arraycopy(children, 0, c, 0, children.length);
      children = c;
    }
    
    if (children[i] != null) {
      children[i].inc_throwAway();
      children[i].parent = null;
    }
    children[i] = node;
    if(i >= numChildren) {
      numChildren = i+1;
    }
    if(node != null) {
      node.setParent(this);
      node.childIndex = i;
    }
  }
  /**
   * @apilevel low-level
   * @declaredat ASTNode:289
   */
  public void insertChild(ASTNode node, int i) {
    
    
    if (!state().IN_CONSTRUCTION && !state().IN_ATTR_STORE_EVAL) {
      if (children == null || i > numChildren) {
        numChildren_handler.notifyDependencies();
      } else {
        numChildren_handler.notifyDependencies();
        getChild_handler.flushRegion();
      }
    
    
    
      ASTNode initial = inc_locateInitialCopy();
      if (initial != null) {
        state().enterConstruction();
        initial.insertChild(node, i);
        state().exitConstruction();
        return;
      }
    
    }
    if(children == null) {
      children = new ASTNode[(i+1>4 || !(this instanceof List))?i+1:4];
      children[i] = node;
    } else {
      ASTNode c[] = new ASTNode[children.length + 1];
      System.arraycopy(children, 0, c, 0, i);
      c[i] = node;
      if(i < children.length) {
        System.arraycopy(children, i, c, i+1, children.length-i);
        for(int j = i+1; j < c.length; ++j) {
          if(c[j] != null) {
            c[j].childIndex = j;
          }
        }
      }
      children = c;
    }
    numChildren++;
    if(node != null) {
      node.setParent(this);
      node.childIndex = i;
    }
  }
  /**
   * @apilevel low-level
   * @declaredat ASTNode:337
   */
  public void removeChild(int i) {
    if(children != null) {
      
      
      if (!state().IN_CONSTRUCTION && !state().IN_ATTR_STORE_EVAL) {
        if (children[i] != null) {
          children[i].inc_notifyForRemove();
        }
        getChild_handler.flushRegion();
      
      
      
        ASTNode initial = inc_locateInitialCopy();
        if (initial != null) {
          state().enterConstruction();
          initial.removeChild(i);
          state().exitConstruction();
          return;
        }
      
      }
      ASTNode child = (ASTNode) children[i];
      if(child != null) {
        
        // prevent recursive call during state handling where setParent calls removeChild
        child.inc_throwAway();
        child.parent = null;
        child.childIndex = -1;
      }
      // Adding a check of this instance to make sure its a List, a move of children doesn't make
      // any sense for a node unless its a list. Also, there is a problem if a child of a non-List node is removed
      // and siblings are moved one step to the right, with null at the end.
      if (this instanceof List || this instanceof Opt) {
        System.arraycopy(children, i+1, children, i, children.length-i-1);
        children[children.length-1] = null;
        numChildren--;
        // fix child indices
        for(int j = i; j < numChildren; ++j) {
          if(children[j] != null) {
            child = (ASTNode) children[j];
            child.childIndex = j;
          }
        }
      } else {
        children[i] = null;
      }
    }
  }
  /**
   * @apilevel low-level
   * @declaredat ASTNode:388
   */
  public ASTNode getParent() {
    if(parent != null && ((ASTNode) parent).is$Final() != is$Final()) {
      state().boundariesCrossed++;
    }
    
    
    state().addHandlerDepTo(getParent_handler);;
    return (ASTNode) parent;
  }
  /**
   * @apilevel low-level
   * @declaredat ASTNode:400
   */
  public void setParent(ASTNode node) {
    if (!state().IN_CONSTRUCTION && !state().IN_ATTR_STORE_EVAL) {
      getParent_handler.notifyDependencies();
    }
    if (parent != null) {
      int index = -1;
      for (int i = 0; parent.children != null && i < parent.children.length; i++) {
        if (parent.children[i] == this) {
          index = i;
          break;
        }
      }
      if (index >= 0) {
        parent.removeChild(index);
      }
    }
    if (node != null) {
      inc_changeState(node.inc_state);
    } else {
      inc_changeState(inc_GARBAGE);
    }
    parent = node;
  }
  /**
   * @apilevel low-level
   * @declaredat ASTNode:473
   */
  public java.util.Iterator<T> iterator() {
    return new java.util.Iterator<T>() {
      private int counter = 0;
      public boolean hasNext() {
        return counter < getNumChild();
      }
      public T next() {
        if(hasNext())
          return (T)getChild(counter++);
        else
          return null;
      }
      public void remove() {
        throw new UnsupportedOperationException();
      }
    };
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:493
   */
  public boolean mayHaveRewrite() {
    return false;
  }
  /**
   * @apilevel low-level
   * @declaredat ASTNode:499
   */
  public void flushTreeCache() {
    flushCache();
    if (children == null) {
      return;
    }
    for (int i = 0; i < children.length; i++) {
      if (children[i] != null && ((ASTNode)children[i]).is$Final) {
        ((ASTNode)children[i]).flushTreeCache();
      }
    }
  }
  /**
   * @apilevel low-level
   * @declaredat ASTNode:513
   */
  public void flushCache() {
    flushRewriteCache();
    flushAttrCache();
    flushCollectionCache();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:521
   */
  public void flushAttrCache() {
    lookup_String_reset();
    localLookup_String_reset();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:528
   */
  public void flushCollectionCache() {
  }
  /**
   * @api internal
   * @declaredat ASTNode:533
   */
  public void flushRewriteCache() {
    for (int i = 0; i < getNumChildNoTransform(); i++) {
      if (children_computed != null && children_computed[i]) {
        if (init_children[i] != null) {
          setChild(init_children[i], i);
          init_children[i] = null;
        }
        children_computed[i] = false;
      }
    }
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:547
   */
  public ASTNode[] init_children;
  /**
   * @apilevel internal
   * @declaredat ASTNode:551
   */
  public boolean[] children_computed;
  /**
   * @apilevel internal
   * @declaredat ASTNode:555
   */
  protected boolean getChild_hasEnclosingInitValue() {
    ASTNode child = this;
    ASTNode parent = this.parent;
    while (parent != null) {
      if (parent.mayHaveRewrite()) {
        return true;
      }
      child = parent;
      parent = parent.parent;
    }
    return false;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:570
   */
  public ASTNode<T> clone() throws CloneNotSupportedException {
    ASTNode node = (ASTNode) super.clone();
    if (node.is$Final()) {
      node.flushCache();
    }
    return node;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:580
   */
  public ASTNode<T> copy() {
    try {
      ASTNode node = (ASTNode) clone();
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
   * @declaredat ASTNode:606
   */
  public ASTNode<T> fullCopy() {
    return treeCopyNoTransform();
  }
  /**
   * Create a deep copy of the AST subtree at this node.
   * The copy is dangling, i.e. has no parent.
   * @return dangling copy of the subtree at this node
   * @apilevel low-level
   * @declaredat ASTNode:615
   */
  public ASTNode<T> treeCopyNoTransform() {
    ASTNode tree = (ASTNode) copy();
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
   * @declaredat ASTNode:636
   */
  public ASTNode<T> treeCopy() {
    doFullTraversal();
    return treeCopyNoTransform();
  }
  /**
   * Performs a full traversal of the tree using getChild to trigger rewrites
   * @apilevel low-level
   * @declaredat ASTNode:644
   */
  public void doFullTraversal() {
    for (int i = 0; i < getNumChild(); i++) {
      getChild(i).doFullTraversal();
    }
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:652
   */
  protected boolean is$Equal(ASTNode n1, ASTNode n2) {
    if (n1 == null && n2 == null) return true;
    if (n1 == null || n2 == null) return false;
    return n1.is$Equal(n2);
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:660
   */
  protected boolean is$Equal(ASTNode node) {
    if (getClass() != node.getClass()) {
      return false;
    }
    if (numChildren != node.numChildren) {
      return false;
    }
    for (int i = 0; i < numChildren; i++) {
      if (children[i] == null && node.children[i] != null) {
        return false;
      }
      if (!((ASTNode)children[i]).is$Equal(((ASTNode)node.children[i]))) {
        return false;
      }
    }
    return true;
  }
  /**
   * @declaredat ASTNode:677
   */
  public String relativeNodeID() {
  StringBuffer res = new StringBuffer();
  ASTNode parent = this.parent;
  int index = -1;
  if (parent != null) {
    res.append(parent.relativeNodeID() + "/");
    for (int i = 0; parent.children != null && i < parent.children.length; i++) {
      if (parent.children[i] != null && parent.children[i] == this && !parent.childIsNTA(i)) {
        index = i;
        break;
      }
    }
  }
  res.append(getClass().getSimpleName());
  if (index > -1) {
    res.append("[" + index + (mayHaveRewrite() ? ",r" : "") + "]");
  }
  return res.toString();
}
  /**
   * @apilevel internal
   * @declaredat ASTNode:699
   */
  protected boolean childIsNTA(int index) {
    return false;
  }
  /**
   * @declaredat ASTNode:702
   */
  protected ASTNode$DepGraphNode getParent_handler = new ASTNode$DepGraphNode(this, "getParent");
  /**
   * @declaredat ASTNode:703
   */
  protected ASTNode$DepGraphNode numChildren_handler = new ASTNode$DepGraphNode(this, "numChildren");
  /**
   * @declaredat ASTNode:704
   */
  protected ASTNode$DepGraphNode getChild_handler = new ASTNode$DepGraphNode(this, "getChild");
  /**
   * @declaredat ASTNode:705
   */
  protected ASTNode$DepGraphNode lookup_String_handler;
  /**
   * @declaredat ASTNode:706
   */
  protected ASTNode$DepGraphNode localLookup_String_handler;
  /**
   * @declaredat ASTNode:707
   */
  protected void inc_copyHandlers(ASTNode copy) {
    // ast handlers
    copy.getChild_handler = new ASTNode$DepGraphNode(getChild_handler, copy);
    copy.numChildren_handler = new ASTNode$DepGraphNode(numChildren_handler, copy);
    copy.getParent_handler = new ASTNode$DepGraphNode(getParent_handler, copy);

        if (lookup_String_handler != null) {
          copy.lookup_String_handler = new ASTNode$DepGraphNode(lookup_String_handler, copy);
        }
        if (localLookup_String_handler != null) {
          copy.localLookup_String_handler = new ASTNode$DepGraphNode(localLookup_String_handler, copy);
        }
  }
  /**
   * @api internal
   * @declaredat ASTNode:723
   */
  protected ASTNode inc_locateEnclosingRewrittenNode() {
     ASTNode child = this;
     ASTNode parent = this.parent;
     while (parent != null) {
       if (child.mayHaveRewrite()) {
         return parent;
       }
       child = parent;
       parent = parent.parent;
     }
     return null;
  }
  /**
   * @api internal
   * @declaredat ASTNode:739
   */
  protected void inc_resetRewrites() {
    for (int i = 0; i < numChildren; i++) {
      if (children_computed != null && i < children_computed.length) {
        children_computed[i] = false;
        if (init_children[i] != null) {
          init_children[i].inc_throwAway();
          init_children[i] = null;
        }
      }
      if (children[i] != null) {
        children[i].inc_resetRewrites();
      }
    }
  }
  /**
   * @api internal
   * @declaredat ASTNode:756
   */
  protected boolean inc_restoreInitialForIndex(int index, ASTNode$DepGraphNode h) {
    if (init_children != null && index < init_children.length && init_children[index] != null) {
      ASTNode oldNode = children[index];
      state().enterConstruction();
      setChild(init_children[index], index);
      state().exitConstruction();
      oldNode.inc_flush_subtree(h);
      init_children[index] = null;
      children_computed[index] = false;
      return true;
    }
    return false;
  }
  /**
   * @api internal
   * @declaredat ASTNode:773
   */
  protected ASTNode inc_locateInitialCopy() {
    // locate enclosing rewrite
    ASTNode child = this;
    ASTNode parent = getParent();
    java.util.LinkedList indexList = new java.util.LinkedList();
    while (parent != null) {
      int index = child.childIndex;
      indexList.addFirst(new Integer(index));
      if (parent.init_children != null && index >= 0 && index < parent.init_children.length && parent.init_children[index] != null) {
        break;
      }
      child = parent;
      parent = child.getParent();
    }
    // root reached -- no enclosing rewrite
    if (parent == null) {
      return null;
    }
    // root not reached -- enclosing rewrite found
    boolean first = true;
    for (java.util.Iterator itr = indexList.iterator(); itr.hasNext();) {
      int index = ((Integer)itr.next()).intValue();
      if (first) {
        first = false;
        child = parent.init_children[index];
        parent = child;
      } else if (index < parent.getNumChildNoTransform()) {
        child = parent.getChildNoTransform(index);
        parent = child;
      } else {
        return null;
      }
    }
    // initial change point found
    return child;
  }
  /**
   * @api internal
   * @declaredat ASTNode:814
   */
  public void reactToDependencyChange(String attrID) {
    
    // flush children
    
    if (attrID.equals("getChild")) {
      if (inc_checkRegionForInnerRewrite()) {
        ASTNode enclosingNode = inc_locateEnclosingRewrittenNode();
        enclosingNode.getChild_handler.flushRegion();
        return;
      } else {
        inc_flushRegion_rewrites();
        getChild_handler.notifyDependencies();
        return;
      }
    }
    
    
    
    
    if (attrID.equals("lookup_String") && lookup_String_values != null && !lookup_String_values.isEmpty()) {
      lookup_String_values = null;
      lookup_String_handler.notifyDependencies();
      return;
    }
    else 
    if (attrID.equals("localLookup_String") && localLookup_String_values != null && !localLookup_String_values.isEmpty()) {
      localLookup_String_values = null;
      localLookup_String_handler.notifyDependencies();
      return;
    }
  }
  /**
   * @api internal
   * @declaredat ASTNode:854
   */
  public boolean inc_checkRegionForInnerRewrite() {
    for (int i = 0; children != null && children_computed != null && i < children.length && i < children_computed.length; i++) {
      ASTNode child = children[i];
      if (child != null) {
        if (children_computed[i]) {
          if (init_children != null && i < init_children.length && init_children[i] == null) {
            return true;
          }
        }
      }
    }
    return false;
  }
  /**
   * @api internal
   * @declaredat ASTNode:873
   */
  public boolean inc_flushRegion_rewrites() {
    for (int i = 0; children != null && i < children.length; i++) {
      ASTNode child = children[i];
      if (child == null) {
        continue;
      }
      // rewritten child
      if (children_computed != null && i < children_computed.length && children_computed[i]) {
        if (!inc_restoreInitialForIndex(i, getChild_handler)) {
          return false;
        }
      }
    }
    return true;
  }
  /**
   * @api internal
   * @declaredat ASTNode:895
   */
  protected void inc_flush_subtree(ASTNode$DepGraphNode h) {
    inc_state = inc_GARBAGE;
    h.transferSetsFrom(lookup_String_handler);
    
    h.transferSetsFrom(localLookup_String_handler);
    // flush subtree of values, take sets from encountered handler and remove it from DDG
    h.transferSetsFrom(getParent_handler);
    h.transferSetsFrom(getChild_handler);
    h.transferSetsFrom(numChildren_handler);
    for (int i = 0; children != null && i < children.length; i++) {
      ASTNode child = children[i];
      if (child != null) {
        child.inc_flush_subtree(h);
      }
    }
  }
  /**
   * @api internal
   * @declaredat ASTNode:920
   */
  public void inc_notifyForRemove() {
    getParent_handler.notifyDependencies();
  }
  /**
   * @declaredat ASTNode:925
   */
  public static final int inc_CREATED = 0;
  /**
   * @declaredat ASTNode:926
   */
  public static final int inc_CLONED = 1;
  /**
   * @declaredat ASTNode:927
   */
  public static final int inc_LIVE = 2;
  /**
   * @declaredat ASTNode:928
   */
  public static final int inc_GARBAGE = 3;
  /**
   * @declaredat ASTNode:929
   */
  public int inc_state = inc_CREATED;
  /**
   * @apilevel internal
   * @declaredat ASTNode:933
   */
  public void inc_changeState(int newState) {
  inc_state = newState;


  getParent_handler.changeState(newState);
  numChildren_handler.changeState(newState);
  getChild_handler.changeState(newState);
  for (int i = 0; children != null && i < children.length; i++) {
    ASTNode child = children[i];
    if (child != null) {
      child.inc_changeState(newState);
    }
  }



  if (lookup_String_handler != null) {
    lookup_String_handler.changeState(newState);
  }
  if (localLookup_String_handler != null) {
    localLookup_String_handler.changeState(newState);
  }
}
  /**
   * @apilevel internal
   * @declaredat ASTNode:959
   */
  public void inc_throwAway() {
  inc_state = inc_GARBAGE;
  getParent_handler.throwAway();
  numChildren_handler.throwAway();
  getChild_handler.throwAway();
  for (int i = 0; children != null && i < children.length; i++) {
    ASTNode child = children[i];
    if (child != null) {
      child.inc_throwAway();
    }
    if (init_children != null && i < init_children.length && init_children[i] != null) {
      init_children[i].inc_throwAway();
    }
  }

  if (lookup_String_handler != null) {
    lookup_String_handler.throwAway();
  }
  if (localLookup_String_handler != null) {
    localLookup_String_handler.throwAway();
  }
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
    Declaration lookup_String_value = null;
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
  public Declaration localLookup(String name) {
    Object _parameters = name;
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
    Declaration localLookup_String_value = null;
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
  public ASTNode rewriteTo() {    if(state().peek() == ASTNode$State.REWRITE_CHANGE) {
      state().pop();
      state().push(ASTNode$State.REWRITE_NOCHANGE);
    }
    return this;
  }  /**
   * @apilevel internal
   */
  public Declaration Define_Declaration_lookupRef(ASTNode caller, ASTNode child, String refName) {
    return getParent().Define_Declaration_lookupRef(this, caller, refName);
  }
  /**
   * @apilevel internal
   */
  public CompilationUnit Define_CompilationUnit_CompilationUnit(ASTNode caller, ASTNode child) {
    return getParent().Define_CompilationUnit_CompilationUnit(this, caller);
  }
}
