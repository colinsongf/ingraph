/*
 * generated by Xtext 2.10.0
 */
package ingraph.report.parser.antlr;

import com.google.inject.Inject;
import ingraph.report.parser.antlr.internal.InternalFeatureParser;
import ingraph.report.services.FeatureGrammarAccess;
import org.eclipse.xtext.parser.antlr.AbstractAntlrParser;
import org.eclipse.xtext.parser.antlr.XtextTokenStream;

public class FeatureParser extends AbstractAntlrParser {

	@Inject
	private FeatureGrammarAccess grammarAccess;

	@Override
	protected void setInitialHiddenTokens(XtextTokenStream tokenStream) {
		tokenStream.setInitialHiddenTokens("RULE_WS", "RULE_SL_COMMENT");
	}
	

	@Override
	protected InternalFeatureParser createParser(XtextTokenStream stream) {
		return new InternalFeatureParser(stream, getGrammarAccess());
	}

	@Override 
	protected String getDefaultRuleName() {
		return "Feature";
	}

	public FeatureGrammarAccess getGrammarAccess() {
		return this.grammarAccess;
	}

	public void setGrammarAccess(FeatureGrammarAccess grammarAccess) {
		this.grammarAccess = grammarAccess;
	}
}
