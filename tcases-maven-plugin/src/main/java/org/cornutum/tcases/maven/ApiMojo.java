package org.cornutum.tcases.maven;

import org.cornutum.tcases.openapi.ApiCommand.Options;
import org.cornutum.tcases.openapi.ApiCommand.Options.TransformType;
import org.cornutum.tcases.openapi.ApiCommand;
import static org.cornutum.tcases.maven.MojoUtils.*;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.plexus.util.DirectoryScanner;
import static org.apache.commons.io.FilenameUtils.getBaseName;
import static org.apache.commons.io.FilenameUtils.getExtension;
import static org.apache.commons.io.FilenameUtils.getPath;

import java.io.File;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * Runs Tcases for OpenAPI. For full details on the Tcases for OpenAPI &mdash; what it does and how it works &mdash; see
 * <A href="https://github.com/Cornutum/tcases/blob/master/tcases-openapi/README.md#tcases-for-openapi-from-rest-ful-to-test-ful">
 * Tcases for OpenAPI: From REST-ful to Test-ful</A>.
 */
@Mojo(name="api")
public class ApiMojo extends AbstractMojo
  {
  @Override
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

      // Generate requested models for each API definition
      String[] apiDefs = inputScanner.getIncludedFiles();
      for( int i = 0; i < apiDefs.length; i++)
        {
        // Define input path for this API definition.
        String inputFile = apiDefs[i];
        File apiDef = new File( inputRootDir, inputFile);

        // Set generator options for this API definition.
        Options options = new Options();
        options.setApiDef( apiDef);
        options.setSource( getSource());
        options.setContentType( getContentType());
        options.setOutDir( new File( getOutDirFile(), getPath( inputFile)));
        if( isJunit())
          {
          options.setTransformType( TransformType.JUNIT);
          }
        if( isHtml())
          {
          options.setTransformType( TransformType.HTML);
          }
        if( getTransformDef() != null)
          {
          options.setTransformType( TransformType.CUSTOM);
          options.setTransformParams( getTransformParams());
          }
        options.setTests( !isInputModels());
        options.setOnModellingCondition( "log".equals( getOnModellingCondition())? getOnCondition() : getOnModellingCondition());
        options.setReadOnlyEnforced( isReadOnlyEnforced());
        options.setWriteOnlyEnforced( isWriteOnlyEnforced());
        
        // Generate requested request test models for this API definition
        if( isRequestCases())
          {
          options.setRequestCases( true);
          options.setOnResolverCondition( getOnResolverCondition());
          options.setMaxTries( getMaxTries());
          options.setRandomSeed( getRandom());
          }
        else
          {
          options.setServerTest( true);
          options.setTransformDef( resolveTransformDefFile( apiDef, options.isServerTest()));
          options.setOutFile( resolveTransformOutFile( apiDef, options.isServerTest()));
          }
        ApiCommand.run( options);

        // Generate requested response test models for this API definition
        options.setServerTest( false);
        options.setTransformDef( resolveTransformDefFile( apiDef, options.isServerTest()));
        options.setOutFile( resolveTransformOutFile( apiDef, options.isServerTest()));
        ApiCommand.run( options);
        }
      }
    catch( Exception e)
      {
      throw new MojoExecutionException( "Can't generate requested models", e);
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
   * Changes if using the JUnit transform.
   */
  public void setJunit( boolean junit)
    {
    this.junit = junit;
    }

  /**
   * Returns if using the JUnit transform.
   */
  public boolean isJunit()
    {
    return junit;
    }

  /**
   * Changes if using the HTML transform.
   */
  public void setHtml( boolean html)
    {
    this.html = html;
    }

  /**
   * Returns if using the HTML transform.
   */
  public boolean isHtml()
    {
    return html;
    }

  /**
   * Changes the transform file.
   */
  public void setTransformDef( String transformDef)
    {
    this.transformDef = transformDef;
    }

  /**
   * Returns the transform file.
   */
  public String getTransformDef()
    {
    return transformDef;
    }

  /**
   * Returns the resolved XSLT transform file path for the given test case perspective.
   */
  private File resolveTransformDefFile( File apiDef, boolean isServerTest)
    {
    return
      Optional.ofNullable( getTransformDef())
      .map( path -> resolveTransformPath( path, apiDef, isServerTest))
      .map( path -> path.isAbsolute()? path : new File( apiDef.getParent(), path.getPath()))
      .orElse( null);
    }

  /**
   * Returns the resolved XSLT transform output path for the given test case perspective.
   */
  private File resolveTransformOutFile( File apiDef, boolean isServerTest)
    {
    return
      Optional.ofNullable( getTransformOutFile())
      .map( path -> resolveTransformPath( path, apiDef, isServerTest))
      .map( path ->
            "java".equals( getExtension( path.getName()))
            ? new File( path.getParent(), getBaseName( path.getName()).replaceAll( "\\W+", "") + ".java")
            : path)
      .orElse( null);
    }

  /**
   * Returns the resolved XSLT transform file path for the given test case perspective.
   */
  private File resolveTransformPath( String path, File apiDef, boolean isServerTest)
    {
    return
      new File(
        path
        .replaceAll( "\\$B", getBaseName( apiDef.getName()))
        .replaceAll( "\\$P", isServerTest? "Requests" : "Responses"));
    }

  /**
   * Changes the transform parameter bindings.
   */
  public void setTransformParams( Map<String,Object> params)
    {
    this.transformParams = params;
    }

  /**
   * Returns the transform parameter bindings.
   */
  public Map<String,Object> getTransformParams()
    {
    return transformParams;
    }

  /**
   * Changes the transform output file name.
   */
  public void setTransformOutFile( String transformOutFile)
    {
    this.transformOutFile = transformOutFile;
    }

  /**
   * Returns the transform output file name.
   */
  public String getTransformOutFile()
    {
    return transformOutFile;
    }

  /**
   * Changes if generating Tcases input models.
   */
  public void setInputModels( boolean inputModels)
    {
    this.inputModels = inputModels;
    }

  /**
   * Returns if generating Tcases input models.
   */
  public boolean isInputModels()
    {
    return inputModels;
    }

  /**
   * Changes how input modelling conditions are handled.
   */
  public void setOnCondition( String onCondition)
    {
    this.onCondition = onCondition;
    }

  /**
   * Returns how input modelling conditions are handled.
   */
  public String getOnCondition()
    {
    return onCondition;
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
   * Changes if the API will strictly enforce the exclusion of "writeOnly" properties from responses. 
   */
  public void setWriteOnlyEnforced( boolean writeOnlyEnforced)
    {
    this.writeOnlyEnforced = writeOnlyEnforced;
    }

  /**
   * Returns if the API will strictly enforce the exclusion of "writeOnly" properties from responses. 
   */
  public boolean isWriteOnlyEnforced()
    {
    return writeOnlyEnforced;
    }

  /**
   * Changes if resolved request test cases will be produced for API requests. 
   */
  public void setRequestCases( boolean requestCases)
    {
    this.requestCases = requestCases;
    }

  /**
   * Returns if resolved request test cases will be produced for API requests.
   */
  public boolean isRequestCases()
    {
    return requestCases;
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
   * Defines the path to the directory where Tcases for OpenAPI output is written.
   * A relative path is applied relative to the <B><CODE>${project.build.directory}</CODE></B> of
   * the project.
   */
  @Parameter(property="outDir",defaultValue="${project.build.directory}/tcases/openapi")
  private String outDir;

  /**
   * If true, generate test cases in the form of JUnit/TestNG test methods.
   */
  @Parameter(property="junit",defaultValue="false")
  private boolean junit;

  /**
   * If true, generate test cases in the form of an HTML report.
   */
  @Parameter(property="html",defaultValue="false")
  private boolean html;

  /**
   * Defines the path to an XSLT transform file applied to the XML form of generated test cases.
   * A relative path is applied relative to the directory containing the corresponding OpenAPI
   * definition file. 
   * This path name may contain references to the certain substitution parameters.
   * Any occurrence of "$B" (the <EM>base</EM>) will be replaced by the base name of the corresponding OpenAPI definition file.
   * Any occurrence of "$P" (the <EM>perspective</EM>) will be replaced by "Requests" or "Responses"
   * when generating test cases for API requests or responses, respectively.
   */
  @Parameter(property="transformDef")
  private String transformDef;

  /**
   * Defines values for the transform parameters used by the transform file.
   */
  @Parameter(property="transformParams")
  private Map<String,Object> transformParams;

  /**
   * Defines name of the output file produced when applying an XSLT transform to generated test cases
   * &mdash; see the <B><CODE>transformDef</CODE></B> parameter for details.
   * This file name may contain references to the certain substitution parameters.
   * Any occurrence of "$B" (the <EM>base</EM>) will be replaced by the base name of the corresponding OpenAPI definition file.
   * Any occurrence of "$P" (the <EM>perspective</EM>) will be replaced by "Requests" or "Responses"
   * when generating test cases for API requests or responses, respectively.
   */
  @Parameter(property="transformOutFile")
  private String transformOutFile;

  /**
   * If true, generate Tcases input models for each OpenAPI definition. Otherwise, generate test case models.
   */
  @Parameter(property="inputModels",defaultValue="false")
  private boolean inputModels;

  /**
   * (Obsolete) Defines how input modelling conditions are handled.
   *
   * @deprecated Replace with "onModellingCondition"
   */
  @Parameter(property="onCondition",defaultValue="log")
  private String onCondition;

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
   * Defines if the API will strictly enforce the exclusion of "writeOnly" properties from responses. 
   */
  @Parameter(property="writeOnlyEnforced",defaultValue="false")
  private boolean writeOnlyEnforced;

  /**
   * Defines if resolved request test cases will be produced for API requests. 
   */
  @Parameter(property="requestCases",defaultValue="false")
  private boolean requestCases;

  /**
   * Defines the source of API input definitions. Valid values are "schemas", or "examples".
   */
  @Parameter(property="source",defaultValue="schemas")
  private String source;

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

  @Parameter(readonly=true,defaultValue="${basedir}")
  private File baseDir_;

  @Parameter(readonly=true,defaultValue="${project.build.directory}")
  private File targetDir_;
  }
