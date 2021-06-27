//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.maven;

import org.cornutum.tcases.openapi.ApiTestCommand.Options;
import org.cornutum.tcases.openapi.ApiTestCommand;
import static org.cornutum.tcases.maven.MojoUtils.*;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.plexus.util.DirectoryScanner;
import static org.apache.commons.io.FilenameUtils.getPath;

import java.io.File;
import java.util.Optional;
import java.util.Set;

/**
 * Generates executable test code for API servers, based on an OpenAPI v3 compliant API definition.
 * For full details on the Tcases for OpenAPI &mdash; what it does and how it works &mdash; see
 * <A href="https://github.com/Cornutum/tcases/blob/master/tcases-openapi/README.md#tcases-for-openapi-from-rest-ful-to-test-ful">
 * Tcases for OpenAPI: From REST-ful to Test-ful</A>.
 */
@Mojo(name="api-test")
public class ApiTestMojo extends AbstractMojo
  {
  public void execute() throws MojoExecutionException
    {
    try
      {
      // Gather API definition files
      DirectoryScanner inputScanner = new DirectoryScanner();
      Set<String> apiDefPatterns = getApiDefs();
      if( !apiDefPatterns.isEmpty())
        {
        // Use all specified API definition patterns.
        }
      else if( !StringUtils.isBlank( getProject()))
        {
        // Use API definition(s) for specified project name.
        apiDefPatterns.add( "**/" + getProject() + ".json");
        apiDefPatterns.add( "**/" + getProject() + ".yaml");
        apiDefPatterns.add( "**/" + getProject() + ".yml");
        }
      else if( !StringUtils.isBlank( getApiDef())) 
        {
        // Use specified API definition pattern.
        apiDefPatterns.add( getApiDef());
        }
      else
        {
        // Use default patterns
        apiDefPatterns.add( "**/*.json");
        apiDefPatterns.add( "**/*.yaml");
        apiDefPatterns.add( "**/*.yml");
        }
      inputScanner.setIncludes( apiDefPatterns.toArray( new String[0]));

      File inputRootDir = getInputDirFile();
      inputScanner.setBasedir( inputRootDir);
      inputScanner.scan();

      // Generate a test for each API definition
      String[] apiDefs = inputScanner.getIncludedFiles();
      for( int i = 0; i < apiDefs.length; i++)
        {
        // Define input path for this API definition.
        String inputFile = apiDefs[i];
        File apiDef = new File( inputRootDir, inputFile);

        // Set generator options for this API definition.
        Options options = new Options();
        options.setApiSpec( apiDef);
        options.setSource( getSource());
        options.setTestType( getTestType());
        options.setExecType( getExecType());
        options.setTestName( getTestName());
        options.setTestPackage( getTestPackage());
        options.setBaseClass( getBaseClass());
        options.setTimeout( getTimeout());
        options.setMocoTestConfig( getMocoTestConfigFile());
        options.setByPath( getByPath());
        options.setPaths( getPaths());
        options.setOperations( getOperations());
        options.setServerUri( getBaseUri());
        options.setContentType( getContentType());
        options.setOutDir( new File( getOutDirFile(), getPath( inputFile)));
        options.setOnModellingCondition( getOnModellingCondition());
        options.setOnResolverCondition( getOnResolverCondition());
        options.setReadOnlyEnforced( isReadOnlyEnforced());
        options.setMaxTries( getMaxTries());
        options.setRandomSeed( getRandom());

        ApiTestCommand.run( options);
        }
      }
    catch( Exception e)
      {
      throw new MojoExecutionException( "Can't generate requested tests", e);
      }
    }

  /**
   * If the given path is not absolute, returns it as an absolute path relative to the
   * project base directory. Otherwise, returns the given absolute path.
   */
  private File getBaseDir( File path)
    {
    return getDirPath( baseDir_, path);
    }

  /**
   * If the given path is not absolute, returns it as an absolute path relative to the
   * project target directory. Otherwise, returns the given absolute path.
   */
  private File getTargetDir( File path)
    {
    return getDirPath( targetDir_, path);
    }

  /**
   * Changes the OpenAPI definition paths.
   */
  public void setApiDefs( Set<String> apiDefs)
    {
    this.apiDefs = apiDefs;
    }

  /**
   * Returns the OpenAPI definition paths.
   */
  public Set<String> getApiDefs()
    {
    return apiDefs;
    }

  /**
   * Changes the OpenAPI definition path.
   */
  public void setApiDef( String apiDef)
    {
    this.apiDef = apiDef;
    }

  /**
   * Returns the OpenAPI definition path.
   */
  public String getApiDef()
    {
    return apiDef;
    }

  /**
   * Changes the OpenAPI definition project name.
   */
  public void setProject( String project)
    {
    this.project = project;
    }

  /**
   * Returns the OpenAPI definition project name.
   */
  public String getProject()
    {
    return project;
    }

  /**
   * Changes the directory that contains OpenAPI definitions.
   */
  public void setInputDir( String inputDir)
    {
    this.inputDir = inputDir;
    }

  /**
   * Returns the directory that contains OpenAPI definitions.
   */
  public String getInputDir()
    {
    return inputDir;
    }

  /**
   * Returns the directory that contains OpenAPI definitions.
   */
  public File getInputDirFile()
    {
    return getBaseDir( getInputDir()==null? null : new File( getInputDir()));
    }

  /**
   * Changes the default content type.
   */
  public void setContentType( String contentType)
    {
    this.contentType = contentType;
    }

  /**
   * Returns the default content type.
   */
  public String getContentType()
    {
    return contentType;
    }

  /**
   * Changes the output directory for generated Tcases models.
   */
  public void setOutDir( String outDir)
    {
    this.outDir = outDir;
    }

  /**
   * Returns the output directory for generated Tcases models.
   */
  public String getOutDir()
    {
    return outDir;
    }

  /**
   * Returns the output directory for generated Tcases models.
   */
  public File getOutDirFile()
    {
    return getTargetDir( outDir==null? null : new File( outDir));
    }

  /**
   * Changes how input modelling conditions are handled.
   */
  public void setOnModellingCondition( String onCondition)
    {
    this.onModellingCondition = onCondition;
    }

  /**
   * Returns how input modelling conditions are handled.
   */
  public String getOnModellingCondition()
    {
    return onModellingCondition;
    }

  /**
   * Changes how request case resolver conditions are handled.
   */
  public void setOnResolverCondition( String onCondition)
    {
    this.onResolverCondition = onCondition;
    }

  /**
   * Returns how request case resolver conditions are handled.
   */
  public String getOnResolverCondition()
    {
    return onResolverCondition;
    }

  /**
   * Changes if the API will strictly enforce the exclusion of "readOnly" properties from requests. 
   */
  public void setReadOnlyEnforced( boolean readOnlyEnforced)
    {
    this.readOnlyEnforced = readOnlyEnforced;
    }

  /**
   * Returns if the API will strictly enforce the exclusion of "readOnly" properties from requests. 
   */
  public boolean isReadOnlyEnforced()
    {
    return readOnlyEnforced;
    }

  /**
   * Changes the maximum attempts made to resolve a request test case input value before reporting failure.
   */
  public void setMaxTries( int maxTries)
    {
    this.maxTries = maxTries;
    }

  /**
   * Returns the maximum attempts made to resolve a request test case input value before reporting failure.
   */
  public int getMaxTries()
    {
    return maxTries;
    }

  /**
   * Changes the random number generator seed for request case resolution.
   */
  public void setRandom( Long random)
    {
    this.random = random;
    }

  /**
   * Returns the random number generator seed for request case resolution.
   */
  public Long getRandom()
    {
    return random;
    }

  /**
   * Changes the source of API input definitions.
   */
  public void setSource( String source)
    {
    this.source = source;
    }

  /**
   * Returns the source of API input definitions.
   */
  public String getSource()
    {
    return source;
    }

  /**
   * Changes the test framework used to run API tests.
   */
  public void setTestType( String testType)
    {
    this.testType = testType;
    }

  /**
   * Returns the test framework used to run API tests.
   */
  public String getTestType()
    {
    return testType;
    }

  /**
   * Changes the request execution intervce used to run API tests.
   */
  public void setExecType( String execType)
    {
    this.execType = execType;
    }

  /**
   * Returns the request execution intervce used to run API tests.
   */
  public String getExecType()
    {
    return execType;
    }

  /**
   * Changes the name of the test class that is generated.
   */
  public void setTestName( String testName)
    {
    this.testName = testName;
    }

  /**
   * Returns the name of the test class that is generated.
   */
  public String getTestName()
    {
    return testName;
    }

  /**
   * Changes the package of the test class that is generated.
   */
  public void setTestPackage( String testPackage)
    {
    this.testPackage = testPackage;
    }

  /**
   * Returns the package of the test class that is generated.
   */
  public String getTestPackage()
    {
    return testPackage;
    }

  /**
   * Changes the base class for the test class that is generated.
   */
  public void setBaseClass( String baseClass)
    {
    this.baseClass = baseClass;
    }

  /**
   * Returns the base class for the test class that is generated.
   */
  public String getBaseClass()
    {
    return baseClass;
    }

  /**
   * Changes the test timeout (milliseconds).
   */
  public void setTimeout( Long millis)
    {
    timeout = millis;
    }

  /**
   * Returns the test timeout (milliseconds).
   */
  public Long getTimeout()
    {
    return timeout;
    }

  /**
   * Changes the Moco server test configuration file
   */
  public void setMocoTestConfig( String mocoTestConfig)
    {
    this.mocoTestConfig = mocoTestConfig;
    }

  /**
   * Returns the Moco server test configuration file
   */
  public String getMocoTestConfig()
    {
    return mocoTestConfig;
    }

  /**
   * Returns the Moco server test configuration file
   */
  public File getMocoTestConfigFile()
    {
    return getBaseDir( getMocoTestConfig()==null? null : new File( getMocoTestConfig()));
    }

  /**
   * If true, a separate test file is generated for each of the API resource paths specified by the {@link #getPaths} option,
   * each containing tests for a single path. Otherwise, a single test file is generated containing tests for all paths.
   */
  public void setByPath( boolean byPath)
    {
    this.byPath = byPath;
    }

  /**
   * If true, a separate test file is generated for each of the API resource paths specified by the {@link #getPaths} option,
   * each containing tests for a single path. Otherwise, a single test file is generated containing tests for all paths.
   */
  public boolean getByPath()
    {
    return byPath;
    }

  /**
   * Changes request paths for which tests are generated.
   */
  public void setPaths( Set<String> paths)
    {
    this.paths =
      Optional.ofNullable( paths)
      .filter( set -> !set.isEmpty())
      .orElse( null);
    }

  /**
   * Returns request paths for which tests are generated.
   */
  public Set<String> getPaths()
    {
    return paths;
    }

  /**
   * Changes request path operations for which tests are generated.
   */
  public void setOperations( Set<String> operations)
    {
    this.operations = 
      Optional.ofNullable( operations)
      .filter( set -> !set.isEmpty())
      .orElse( null);
    }

  /**
   * Returns request path operations for which tests are generated.
   */
  public Set<String> getOperations()
    {
    return operations;
    }

  /**
   * Changes the expression that identifies the API server URI used by generated tests.
   */
  public void setBaseUri( String serverExpr)
    {
    this.baseUri = serverExpr;
    }

  /**
   * Returns the expression that identifies the API server URI used by generated tests.
   */
  public String getBaseUri()
    {
    return baseUri;
    }

  /**
   * Defines a set of patterns that match the OpenAPI definition files read by Tcases for OpenAPI.
   * By default, Tcases for OpenAPI uses the single pattern defined by the <B><CODE>apiDef</CODE></B> parameter.
   */
  @Parameter(property="apiDefs")
  private Set<String> apiDefs;

  /**
   * Defines a single pattern that matches the OpenAPI definition files read by Tcases for OpenAPI,
   * relative to the directory specified by the <B><CODE>inputDir</CODE></B>.
   * If omitted, the default value matches all files of the form "*.json", "*.yaml", or "*.yml".
   */
  @Parameter(property="apiDef")
  private String apiDef;
  
  /**
   * A short-hand form of the <B><CODE>apiDefs</CODE></B> parameter that makes it easier
   * to select the OpenAPI definition for a specific project. Equivalent to setting
   * <B><CODE>apiDefs</CODE></B> to
   * <CODE>&lowast;&lowast;/${project}.json,&lowast;&lowast;/${project}.yaml,&lowast;&lowast;/${project}.yml</CODE>.
   */
  @Parameter(property="project")
  private String project;

  /**
   * Defines the path to the directory where OpenAPI definition files are located.
   * A relative path is applied relative to the <B><CODE>${basedir}</CODE></B> of
   * the project.
   */
  @Parameter(property="inputDir",defaultValue="${basedir}/src/test/tcases/openapi")
  private String inputDir;

  /**
   * Defines the default content type for API definition files. The <B><CODE>contentType</CODE></B> must be one of "json" or "yaml".
   * The default content type is assumed for any file that is not specified explicitly or that does not have a recognized extension.
   * If omitted, the default content type is "json".
   */
  @Parameter(property="contentType")
  private String contentType;

  /**
   * Defines the path to the directory where generated tests are written.
   * A relative path is applied relative to the <B><CODE>${project.build.directory}</CODE></B> of
   * the project.
   */
  @Parameter(property="outDir",defaultValue="${project.build.directory}/generated-test-sources/java")
  private String outDir;

  /**
   * Defines how input modelling conditions are handled. Valid values are "log", "fail", or "ignore".
   */
  @Parameter(property="onModellingCondition",defaultValue="log")
  private String onModellingCondition;

  /**
   * Defines how request case resolver conditions are handled. Valid values are "log", "fail", or "ignore".
   */
  @Parameter(property="onResolverCondition",defaultValue="log")
  private String onResolverCondition;

  /**
   * Defines if the API will strictly enforce the exclusion of "readOnly" properties from requests. 
   */
  @Parameter(property="readOnlyEnforced",defaultValue="false")
  private boolean readOnlyEnforced;

  /**
   * Defines the maximum attempts made to resolve a request test case input value before reporting failure.
   */
  @Parameter(property="maxTries",defaultValue="10000")
  private int maxTries;

  /**
   * Defines the random number generator seed for request case resolution.
   */
  @Parameter(property="random")
  private Long random;

  /**
   * Defines the source of API input definitions. Valid values are "schemas", or "examples".
   */
  @Parameter(property="source",defaultValue="schemas")
  private String source;

  /**
   * Defines the test framework used to run API tests. Valid values are "junit", "testng", or "moco".
   */
  @Parameter(property="testType",defaultValue="junit")
  private String testType;

  /**
   * Defines the request execution interface used to run API tests. Valid values are "restassured".
   */
  @Parameter(property="execType",defaultValue="restassured")
  private String execType;

  /**
   * Defines the name of the test class that is generated. This can be either a fully-qualified class name
   * or a simple class name.
   */
  @Parameter(property="testName")
  private String testName;

  /**
   * Defines the package for the test class that is generated. This can be omitted if the <B><CODE>testName</CODE></B>
   * is a fully-qualified class name or if the package can be determined from the <B><CODE>outDir</CODE></B>.
   */
  @Parameter(property="testPackage")
  private String testPackage;

  /**
   * If defined, specifies a base class for the generated test class. This can be a fully-qualified class name
   * or a simple class name, if the base class belongs to the same package as the generated test class.
   */
  @Parameter(property="baseClass")
  private String baseClass;

  /**
   * Defines the maximum time (in milliseconds) to complete an individual test method. A test failure occurs if a
   * method continues past this time limit. If omitted, no time limit is enforced.
   */
  @Parameter(property="timeout")
  private Long timeout;

  /**
   * When the <B><CODE>testType</CODE></B> is "moco", specifies the Moco server test configuration file.
   * A relative path is applied relative to the <B><CODE>${basedir}</CODE></B> of
   * the project.
   */
  @Parameter(property="mocoTestConfig")
  private String mocoTestConfig;

  /**
   * If true, a separate test file is generated for each of the API resource paths specified by the <B><CODE>paths</CODE></B> option,
   * each containing tests for a single path. Otherwise, a single test file is generated containing tests for all paths.
   */
  @Parameter(property="byPath",defaultValue="false")
  private boolean byPath;

  /**
   * If defined, tests are generated only for the specified API resource paths. 
   * If omitted, tests are generated for all resource paths.
   */
  @Parameter(property="paths")
  private Set<String> paths;

  /**
   * If defined, tests are generated only for the specified HTTP methods.
   * If omitted, tests are generated for all operations.
   */
  @Parameter(property="operations")
  private Set<String> operations;

  /**
   * If defined, specifies the base URI for the API server used by the generated tests.
   * The value is an expression of one the following forms.
   * If omitted, the default is "index=0".
   * <P/>
   * <UL>
   * <LI> index=<I>&lt;integer&gt;</I>
   * <P>
   * From the servers array defined in the OpenAPI definition, use the URI of the given element.
   * </P>
   * </LI>
   * <LI> contains=<I>&lt;text&gt;</I>
   * <P>
   * From the servers array defined in the OpenAPI definition, use the URI of the first element with a description
   * containing the given text.
   * </P>
   * </LI>
   * <LI> uri=<I>&lt;uri&gt;</I>
   * <P>
   * Use the specified <I>&lt;uri&gt;</I>.
   * </P>
   * </LI>
   * </UL>
   */
  @Parameter(property="baseUri")
  private String baseUri;

  @Parameter(readonly=true,defaultValue="${basedir}")
  private File baseDir_;

  @Parameter(readonly=true,defaultValue="${project.build.directory}")
  private File targetDir_;
  }
