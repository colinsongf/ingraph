/*
 * generated by Xtext 2.10.0
 */
package ingraph.report.featuregrammar


/**
 * Initialization support for running Xtext languages without Equinox extension registry.
 */
class FeatureStandaloneSetup extends FeatureStandaloneSetupGenerated {

	def static void doSetup() {
		new FeatureStandaloneSetup().createInjectorAndDoEMFRegistration()
	}
}
