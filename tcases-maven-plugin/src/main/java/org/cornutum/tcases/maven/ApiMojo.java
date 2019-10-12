package org.cornutum.tcases.maven;

import org.cornutum.tcases.openapi.ApiCommand.Options;
import org.cornutum.tcases.openapi.ApiCommand.Options.TransformType;
import org.cornutum.tcases.openapi.ApiCommand;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.plexus.util.DirectoryScanner;
import static org.apache.commons.io.FilenameUtils.getPath;

import java.io.File;
import java.util.Set;

/**
 * Runs Tcases for OpenAPI. For full details on the Tcases for OpenAPI &mdash; what it does and how it works &mdash; see
 * <A href="https://github.com/Cornutum/tcases/blob/master/tcases-openapi/README.md#tcases-for-openapi-from-rest-ful-to-test-ful">
 * Tcases for OpenAPI: From REST-ful to Test-ful</A>.
 */
@Mojo(name="api")
public class ApiMojo extends AbstractMojo
  {
  public void execute() throws MojoExecutionException
    {
    try
      {
      // Gather API spec files
      DirectoryScanner inputScanner = new DirectoryScanner();
      Set<String> apiDefPatterns = getApiDefs();
      if( !apiDefPatterns.isEmpty())
        {
        // Use all specified API spec patterns.
        }
      else if( !StringUtils.isBlank( getProject()))
        {
        // Use API spec(s) for specified project name.
        apiDefPatterns.add( "**/" + getProject() + ".json");
        apiDefPatterns.add( "**/" + getProject() + ".yaml");
        apiDefPatterns.add( "**/" + getProject() + ".yml");
        }
      else if( !StringUtils.isBlank( getApiDef())) 
        {
        // Use specified API spec pattern.
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

      // Generate requested models for each API spec
      String[] apiDefs = inputScanner.getIncludedFiles();
      for( int i = 0; i < apiDefs.length; i++)
        {
        // Define input path for this API spec.
        String inputFile = apiDefs[i];
        File apiDef = new File( inputRootDir, inputFile);

        // Set generator options for this API spec.
        Options options = new Options();
        options.setApiSpec( apiDef);
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
        options.setTests( !isInputModels());
        options.setOnCondition( getOnCondition());
        options.setReadOnlyEnforced( isReadOnlyEnforced());
        options.setWriteOnlyEnforced( isWriteOnlyEnforced());
        
        // Generate requested models for this API spec
        options.setServerTest( true);
        ApiCommand.run( options);

        options.setServerTest( false);
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
    return
      path == null?
      baseDir_ :
      
      path.isAbsolute()?
      path :

      new File( baseDir_, path.getPath());
    }

  /**
   * If the given path is not absolute, returns it as an absolute path relative to the
   * project target directory. Otherwise, returns the given absolute path.
   */
  private File getTargetDir( File path)
    {
    return
      path == null?
      targetDir_ :

      path.isAbsolute()?
      path :

      new File( targetDir_, path.getPath());
    }

  /**
   * Changes the OpenAPI specification paths.
   */
  public void setApiDefs( Set<String> apiDefs)
    {
    this.apiDefs = apiDefs;
    }

  /**
   * Returns the OpenAPI specification paths.
   */
  public Set<String> getApiDefs()
    {
    return apiDefs;
    }

  /**
   * Changes the OpenAPI specification path.
   */
  public void setApiDef( String apiDef)
    {
    this.apiDef = apiDef;
    }

  /**
   * Returns the OpenAPI specification path.
   */
  public String getApiDef()
    {
    return apiDef;
    }

  /**
   * Changes the OpenAPI specification project name.
   */
  public void setProject( String project)
    {
    this.project = project;
    }

  /**
   * Returns the OpenAPI specification project name.
   */
  public String getProject()
    {
    return project;
    }

  /**
   * Changes the directory that contains OpenAPI specifications.
   */
  public void setInputDir( String inputDir)
    {
    this.inputDir = inputDir;
    }

  /**
   * Returns the directory that contains OpenAPI specifications.
   */
  public String getInputDir()
    {
    return inputDir;
    }

  /**
   * Returns the directory that contains OpenAPI specifications.
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
   * Defines a set of patterns that match the OpenAPI specification files read by Tcases for OpenAPI.
   * By default, Tcases for OpenAPI uses the single pattern defined by the <B><CODE>apiDef</CODE></B> parameter.
   */
  @Parameter(property="apiDefs")
  private Set<String> apiDefs;

  /**
   * Defines a single pattern that matches the OpenAPI specification files read by Tcases for OpenAPI,
   * relative to the directory specified by the <B><CODE>inputDir</CODE></B>.
   * If omitted, the default value matches all files of the form "*.json", "*.yaml", or "*.yml".
   */
  @Parameter(property="apiDef")
  private String apiDef;
  
  /**
   * A short-hand form of the <B><CODE>apiDefs</CODE></B> parameter that makes it easier
   * to select the OpenAPI specification for a specific project. Equivalent to setting
   * <B><CODE>apiDefs</CODE></B> to
   * <CODE>&lowast;&lowast;/${project}.json,&lowast;&lowast;/${project}.yaml,&lowast;&lowast;/${project}.yml</CODE>.
   */
  @Parameter(property="project")
  private String project;

  /**
   * Defines the path to the directory where OpenAPI specification files are located.
   * A relative path is applied relative to the <B><CODE>${basedir}</CODE></B> of
   * the project.
   */
  @Parameter(property="inputDir",defaultValue="${basedir}/src/test/tcases/openapi")
  private String inputDir;

  /**
   * Defines the default content type for API spec files. The <B><CODE>contentType</CODE></B> must be one of "json" or "yaml".
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
   * If true, generate Tcases input models for each OpenAPI specification. Otherwise, generate test case models.
   */
  @Parameter(property="inputModels",defaultValue="false")
  private boolean inputModels;

  /**
   * Defines how input modelling conditions are handled. Valid values are "log", "fail", or "ignore".
   */
  @Parameter(property="onCondition",defaultValue="log")
  private String onCondition;

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

  @Parameter(readonly=true,defaultValue="${basedir}")
  private File baseDir_;

  @Parameter(readonly=true,defaultValue="${project.build.directory}")
  private File targetDir_;
  }
