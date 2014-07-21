/**
 * This program and the accompanying materials are made available under the
 * terms of the MIT license (X11 license) which accompanies this distribution.
 *
 * @author M. Tasić, C. Bürger
 */

package cjava;

import java.io.*;

import cjava.ast.*;

public class Main {
	public static interface Strategy {
		public void execute();
	}
	private static class NullStrategy implements Strategy {
		public void execute() {}
	}
	private static class CheckStrategy implements Strategy {
		public CompilationUnit ast = null;
		public CheckStrategy(CompilationUnit ast) {this.ast=ast;}
		public void execute() {ast.isCorrect();}
	}
	public static Strategy strategy = null;
	
	public static void main(String[] args) {
		if (args.length < 2) {
			System.out.println("Usage: mode, recipe, fragments ...");
			System.exit(1);
		}
		
		try {
			// Parse recipe:
			FileReader recipe_file = new FileReader(args[1]);
			CompositionProgram recipe = null;
			try {
				LexerCL lexer_cl = new LexerCL(recipe_file);
				ParserCL parser_cl = new ParserCL();
				recipe = (CompositionProgram) parser_cl.parse(lexer_cl);
			} finally {recipe_file.close();}
			// Parse fragments:
			List<ClassDeclaration> fragments = new List<ClassDeclaration>();
			for (int i = 2; i < args.length; i++) {
				FileReader fragment_file = new FileReader(args[i]);
				try{
					LexerCJ lexer_cj = new LexerCJ(fragment_file);
					ParserCJ parser_cj = new ParserCJ();
					parser_cj.source = args[i];
					fragments.add((ClassDeclaration) parser_cj.parse(lexer_cj));
				} finally {fragment_file.close();}
			}
			// Perform compositions:
			CompilationUnit comp_unit = new CompilationUnit(fragments, recipe);
			if (args[0].equals("always")) {
				strategy = new CheckStrategy(comp_unit);
				comp_unit.performCompositions();
				strategy.execute();
			} else if (args[0].equals("once")) {
				strategy = new NullStrategy();
				comp_unit.performCompositions();
				strategy = new CheckStrategy(comp_unit);
				strategy.execute();
			} else if (args[0].equals("never")) {
				strategy = new NullStrategy();
				comp_unit.performCompositions();
			} else {
				throw new IllegalArgumentException("Unknown composition mode [" + args[0] + "].");
			}
			// Pretty print fragments:
			for (int i = 0; i < comp_unit.getNumBody(); i++) {
				ClassDeclaration fragment = comp_unit.getBody(i);
				Writer writer = new BufferedWriter(
					new OutputStreamWriter(
					new FileOutputStream(fragment.getSource()), "utf-8"));
				try {
					fragment.pp(writer, 0);
				} finally {writer.close();}
			}
		} catch (Exception exc) {
			exc.printStackTrace();
			System.exit(1);
		}
	}
}
