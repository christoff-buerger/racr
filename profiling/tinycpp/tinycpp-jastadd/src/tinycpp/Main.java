/**
 * This program and the accompanying materials are made available under the
 * terms of the MIT license (X11 license) which accompanies this distribution.
 *
 * @author C. Bürger
 */

package tinycpp;

import java.io.*;

import tinycpp.ast.*;

public class Main {
	public static void main(String[] args) {
		final boolean fail = args.length == 2;
		if (args.length < 1 || args.length > 2 || (fail && !args[0].equals("-v"))) {
			System.out.println("Usage: java -jar tinycpp.jar -v file");
			System.out.println("Help: Transforms the given Tiny C++ program to a normalform if it is well-formed.");
			System.out.println("      Aborts with an error message if it is not well-formed.");
			System.out.println("Options:");
			System.out.println("	-v Success if program is not well-formed.");
			System.exit(2);
		}
		final String program_file = args[fail ? 1 : 0];
		
		try {
			CompilationUnit ast;
			FileReader program = new FileReader(program_file);
			try{
				Lexer lexer = new Lexer(program);
				Parser parser = new Parser();
				ast = (CompilationUnit) parser.parse(lexer);
			} finally {program.close();}
			ast.transformToNormalform();
			if (!ast.IsCorrect())
				throw new TinyCPPException("ERROR: Program not well-formed.");
			Writer writer = new BufferedWriter(
				new OutputStreamWriter(
				new FileOutputStream(program_file), "utf-8"));
			try {
				ast.prettyPrint(writer);
			} finally { writer.close();}
			if (fail)
				throw new RuntimeException("ERROR: Program is well-formed.");
		} catch (TinyCPPException exc) {
			if (fail)
				System.exit(0);
			exc.printStackTrace();
			System.exit(1);
		} catch (Exception exc) {
			exc.printStackTrace();
			System.exit(1);
		}
	}
}
