/*
 * generated by Xtext 2.10.0
 */
package ingraph.report.parser.antlr;

import java.io.InputStream;

import org.eclipse.xtext.parser.antlr.IAntlrTokenFileProvider;

public class FeatureAntlrTokenFileProvider implements IAntlrTokenFileProvider {

	@Override
	public InputStream getAntlrTokenFile() {
		ClassLoader classLoader = getClass().getClassLoader();
		return classLoader.getResourceAsStream("ingraph/report/parser/antlr/internal/InternalFeature.tokens");
	}
}
