package ingraph.debugger.backend.messages.out;

import org.neo4j.cypher.SyntaxException;

public class RegisterParseErrorBody {

	private String message;

	public RegisterParseErrorBody(SyntaxException syntaxExc) {
		this.message = syntaxExc.getMessage();
	}

	public String getMessage() {
		return message;
	}

}
