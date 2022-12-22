package org.cornutum.tcases.maven;

import org.cornutum.tcases.TcasesCommand.Options;
import static org.cornutum.tcases.CommandUtils.*;
import static org.cornutum.tcases.maven.MojoUtils.*;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.plexus.util.DirectoryScanner;
import org.codehaus.plexus.util.FileUtils;
import org.cornutum.tcases.TcasesCommand;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Map;
import java.util.Set;

/**
 * Runs Tcases to generate test cases from one or more system input definition documents.
 * For full details on Tcases &mdash; what it does and how it works &mdash; see
 * <A href="http://www.cornutum.org/tcases/docs/Tcases-Guide.htm">Tcases: The Complete Guide</A> at www.cornutum.org.
 *
 */
@Mojo(name="tcases",defaultPhase=LifecyclePhase.GENERATE_TEST_RESOURCES)
public class TcasesMojo extends AbstractMojo
  {
  @Override
  public void execute() throws MojoExecutionException
    {
    try
      {
      File inputRootDir = getInputDirFile();
      if( !inputRootDir.exists())
        {
        logger_.warn( "Input directory={} does not exist", inputRootDir);
        }

      else
        {
        // Gather input definition files
        DirectoryScanner inputScanner = new DirectoryScanner();
        inputScanner.setBasedir( inputRootDir);

        Set<String> inputDefPatterns = getInputDefs();
        if( !inputDefPatterns.isEmpty())
          {
          // Use all specified input definition patterns.
          }
        else if( !StringUtils.isBlank( getProject()))
          {
          // Use input definition(s) for specified project name.
          inputDefPatterns.add( "**/" + getProject() + "-Input.xml");
          inputDefPatterns.add( "**/" + getProject() + ".xml");
          inputDefPatterns.add( "**/" + getProject() + "-Input.json");
          inputDefPatterns.add( "**/" + getProject() + ".json");
          }
        else if( !StringUtils.isBlank( getInputDef())) 
          {
          // Use specified input definition pattern.
          inputDefPatterns.add( getInputDef());
          }
        else
          {
          // Use default patterns
          inputDefPatterns.add( "**/*-Input.xml");
          inputDefPatterns.add( "**/*-Input.json");
          }
        inputScanner.setIncludes( inputDefPatterns.toArray( new String[0]));
        inputScanner.scan();

        // Generate test cases for each input definition file.
        File outRootDir = getOutDirFile();
        String[] inputDefs = inputScanner.getIncludedFiles();
        for( int i = 0; i < inputDefs.length; i++)
          {
          // Define input and output paths for this Tcases project.
          String inputFile = inputDefs[i];
          String inputPath = FileUtils.dirname( inputFile);
          File inputDef = new File( inputRootDir, inputFile);
          File inputDir = new File( inputRootDir, inputPath);
          File outDir = new File( outRootDir, inputPath);

          String projectName = getProjectName( inputDef);
          String outFile = getOutFile();
          if( outFile != null && (outFile = getProjectFile( projectName, outFile)) == null)
            {
            throw new IllegalArgumentException( "Invalid outFile pattern='" + getOutFile() + "'");
            }

          String testDef = getTestDef();
          if( testDef != null && (testDef = getProjectFile( projectName, testDef)) == null)
            {
            throw new IllegalArgumentException( "Invalid testDef pattern='" + getTestDef() + "'");
            }

          String genDef = getGenDef();
          if( genDef != null && (genDef = getProjectFile( projectName, genDef)) == null)
            {
            throw new IllegalArgumentException( "Invalid genDef pattern='" + getGenDef() + "'");
            }


          // Set options for this Tcases project.
          Options options = new Options();
          options.setInputDef( inputDef);
          options.setGenDef( genDef==null? null : new File( inputDir, genDef));
          options.setOutDir( outDir);
          options.setTestDef( testDef==null? null : new File( inputDir, testDef));
          options.setOutFile( outFile==null? null : new File( outDir, outFile));
          options.setExtended( !isNewTests());
          options.setRandomSeed( getSeed());
          options.setNewSeed( isNewSeed());
          options.setDefaultTupleSize( getTuples());
          options.setContentType( getContentType());
          options.setShowEffectiveInput( isShowEffectiveInput());

          File transformDef = getTransformDefFile();
          if( transformDef != null)
            {
            options.setTransformType( Options.TransformType.CUSTOM);
            String projectTransformDef = getProjectFile( projectName, transformDef.getPath());
            if( projectTransformDef == null)
              {
              throw new IllegalArgumentException( "Invalid transformDef pattern='" + transformDef + "'");
              }
            options.setTransformDef
              ( transformDef.isAbsolute()
                ? new File( projectTransformDef)
                : new File( inputDir, projectTransformDef));
            }
          else if( isJunit())
            {
            options.setTransformType( Options.TransformType.JUNIT);
            }
          else if( isHtml())
            {
            options.setTransformType( Options.TransformType.HTML);
            }
          
          options.setTransformParams( getTransformParams());

          // Generate test cases for this Tcases project.
          TcasesCommand.run( options);
          }
        }
      }
    catch( Exception e)
      {
      throw new MojoExecutionException( "Can't generate test cases", e);
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
   * Changes the system input definition paths.
   */
  public void setInputDefs( Set<String> inputDefs)
    {
    this.inputDefs = inputDefs;
    }

  /**
   * Returns the system input definition paths.
   */
  public Set<String> getInputDefs()
    {
    return inputDefs;
    }

  /**
   * Changes the system input definition path.
   */
  public void setInputDef( String inputDef)
    {
    this.inputDef = inputDef;
    }

  /**
   * Returns the system input definition path.
   */
  public String getInputDef()
    {
    return inputDef;
    }

  /**
   * Changes the system input definition project name.
   */
  public void setProject( String project)
    {
    this.project = project;
    }

  /**
   * Returns the system input definition project name.
   */
  public String getProject()
    {
    return project;
    }

  /**
   * Changes the directory that contains system input definitions.
   */
  public void setInputDir( String inputDir)
    {
    this.inputDir = inputDir;
    }

  /**
   * Returns the directory that contains system input definitions.
   */
  public String getInputDir()
    {
    return inputDir;
    }

  /**
   * Returns the directory that contains system input definitions.
   */
  public File getInputDirFile()
    {
    return getBaseDir( inputDir==null? null : new File( inputDir));
    }

  /**
   * Changes the default tuple size for all generators.
   */
  public void setTuples( Integer tuples)
    {
    this.defaultTupleSize = tuples;
    }

  /**
   * Returns the default tuple size for all generators.
   */
  public Integer getTuples()
    {
    return defaultTupleSize;
    }

  /**
   * Changes the test definition output file pattern.
   */
  public void setOutFile( String outFile)
    {
    this.outFile = outFile;
    }

  /**
   * Returns the test definition output file pattern.
   */
  public String getOutFile()
    {
    return outFile;
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
   * Changes the generator definition path.
   */
  public void setGenDef( String genDef)
    {
    this.genDef = genDef;
    }

  /**
   * Returns the generator definition path.
   */
  public String getGenDef()
    {
    return genDef;
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
   * Changes if the current effective system input definition should be written.
   */
  public void setShowEffectiveInput( boolean showEffectiveInput)
    {
    this.showEffectiveInput = showEffectiveInput;
    }

  /**
   * Returns if the current effective system input definition should be written.
   */
  public boolean isShowEffectiveInput()
    {
    return showEffectiveInput;
    }

  /**
   * Changes if previous contents of the test definition file are ignored.
   * If false, new test definitions are based on the previous test definitions.
   */
  public void setNewTests( boolean newTests)
    {
    this.newTests = newTests;
    }

  /**
   * Returns if previous contents of the test definition file are ignored.
   * If false, new test definitions are based on the previous test definitions.
   */
  public boolean isNewTests()
    {
    return newTests;
    }

  /**
   * Changes the output directory for generated test definitions.
   */
  public void setOutDir( String outDir)
    {
    this.outDir = outDir;
    }

  /**
   * Returns the output directory for generated test definitions.
   */
  public String getOutDir()
    {
    return outDir;
    }

  /**
   * Returns the output directory for generated test definitions.
   */
  public File getOutDirFile()
    {
    return getTargetDir( outDir==null? null : new File( outDir));
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
   * Changes the random seed used by generators.
   */
  public void setSeed( Long seed)
    {
    this.seed = seed;
    }

  /**
   * Returns the random seed used by generators.
   */
  public Long getSeed()
    {
    return seed;
    }

  /**
   * Changes if choosing a new random seed used by generators.
   */
  public void setNewSeed( boolean newSeed)
    {
    this.newSeed = newSeed;
    }

  /**
   * Returns if choosing a new random seed used by generators.
   */
  public boolean isNewSeed()
    {
    return newSeed;
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
   * Returns the transform file.
   */
  public File getTransformDefFile()
    {
    return
      transformDef == null
      ? null
      : new File( transformDef);
    }

  /**

   * Changes the test definition input file pattern.
   */
  public void setTestDef( String testDef)
    {
    this.testDef = testDef;
    }

  /**
   * Returns the test definition input file pattern.
   */
  public String getTestDef()
    {
    return testDef;
    }
  
  /**
   * Defines a set of patterns that match the system input definition files read by Tcases.
   * By default, Tcases uses the single pattern defined by the <B><CODE>inputDef</CODE></B> parameter.
   */
  @Parameter(property="inputDefs")
  private Set<String> inputDefs;

  /**
   * Defines a single pattern that matches the system input definition files read by Tcases,
   * relative to the directory specified by the <B><CODE>inputDir</CODE></B>.
   * If omitted, the default value matches all files of the form "*-Input.xml" or "*-Input.json".
   * <P/>
   * An input definition file defines a <EM>project name</EM> that is used to form the
   * default names for other associated files. For an input definition file of the form
   * "${prefix}-Input.xml", the project name is "${prefix}". Otherwise, the project name
   * is the basename of the input definition file.
   */
  @Parameter(property="inputDef")
  private String inputDef;
  
  /**
   * A short-hand form of the <B><CODE>inputDefs</CODE></B> parameter that makes it easier
   * to select the system input definition for a specific project. Equivalent to setting
   * <B><CODE>inputDefs</CODE></B> to
   * <CODE>&lowast;&lowast;/${project}-Input.xml,&lowast;&lowast;/${project}.xml,&lowast;&lowast;/${project}-Input.json,&lowast;&lowast;/${project}.json</CODE>.
   */
  @Parameter(property="project")
  private String project;

  /**
   * Defines the path to the directory where system input definition files are located.
   * A relative path is applied relative to the <B><CODE>${basedir}</CODE></B> of
   * the project.
   */
  @Parameter(property="inputDir",defaultValue="${basedir}/src/test/tcases")
  private String inputDir;

  /**
   * Defines the path to the directory where Tcases output is written.
   * A relative path is applied relative to the <B><CODE>${project.build.directory}</CODE></B> of
   * the project.
   */
  @Parameter(property="outDir",defaultValue="${project.build.directory}/tcases")
  private String outDir;

  /**
   * Defines the default content type for files that are read and produced. The <B><CODE>contentType</CODE></B> must be one of "json" or "xml".
   * The default content type is assumed for any file that is not specified explicitly or that does not have a recognized extension.
   * If omitted, the default content type is derived from the input definition file name.
   */
  @Parameter(property="contentType")
  private String contentType;

  /**
   * Defines the name for the output file generated by Tcases from an input definition file.
   * This file name may contain at most one "*" wildcard character, in which case
   * the "*" is replaced by the <EM>project name</EM> of the corresponding
   * input definition file &mdash; see the <B><CODE>inputDef</CODE></B> parameter for
   * details.
   * The default value is "*-Test.xml" or "*-Test.json" (depending on the <B><CODE>contentType</CODE></B>)
   * or, if the <B><CODE>junit</CODE></B> parameter is true, "*Test.java".
   */
  @Parameter(property="outFile")
  private String outFile;

  /**
   * Defines the name for the initial test case definition files read by Tcases for an input definition file.
   * This file name may contain at most one "*" wildcard character, in which case
   * the "*" is replaced by the <EM>project name</EM> of the corresponding
   * input definition file &mdash; see the <B><CODE>inputDef</CODE></B> parameter for
   * details.
   * The default value is "*-Test.xml" or "*-Test.json" (depending on the <B><CODE>contentType</CODE></B>).
   */
  @Parameter(property="testDef")
  private String testDef;

  /**
   * Defines the name for the generator definition file read by Tcases for an input definition file.
   * This file name may contain at most one "*" wildcard character, in which case
   * the "*" is replaced by the <EM>project name</EM> of the corresponding
   * input definition file &mdash; see the <B><CODE>inputDef</CODE></B> parameter for
   * details.
   * The default value is "*-Generators.xml" or "*-Generators.json" (depending on the <B><CODE>contentType</CODE></B>).
   */
  @Parameter(property="genDef")
  private String genDef;

  /**
   * Defines the path to the transform file applied by Tcases to the output for an input definition file.
   * A relative path is applied relative to the directory containing the corresponding input
   * definition file. 
   * This path name may contain at most one "*" wildcard character, in which case the "*"
   * is replaced by the <EM>project name</EM> of the corresponding input definition file &mdash;
   * see the <B><CODE>inputDef</CODE></B> parameter for details.
   */
  @Parameter(property="transformDef")
  private String transformDef;

  /**
   * Defines values for the transform parameters used by the transform file.
   */
  @Parameter(property="transformParams")
  private Map<String,Object> transformParams;

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
   * If true, no test definitions are produced. Instead, a JSON document containing the effective system input definition is
   * written to the <B><CODE>outDir</CODE></B>. The effective system input definition, which is used to generate test
   * definitions, is the result of normalizing all input schemas and adding any schema-derived value definitions.
   */
  @Parameter(property="showEffectiveInput",defaultValue="false")
  private boolean showEffectiveInput;

  /**
   * If true, ignore any initial test definitions. Otherwise, generate new test definitions
   * that extend the initial test definitions found in the file defined by the <B><CODE>testDef</CODE></B>
   * parameter.
   */
  @Parameter(property="newTests",defaultValue="false")
  private boolean newTests;

  /**
   * If defined, use the given random number for all generators. This updates the generator definitions specified
   * by the <B><CODE>genDef</CODE></B> parameter.
   */
  @Parameter(property="seed")
  private Long seed;

  /**
   * If true, choose a new random number for all generators. This updates the generator definitions specified
   * by the <B><CODE>genDef</CODE></B> parameter.
   */
  @Parameter(property="newSeed",defaultValue="false")
  private boolean newSeed;

  /**
   * If defined, use the given default tuple size for all generators. This updates the generator definitions specified
   * by the <B><CODE>genDef</CODE></B> parameter.
   */
  @Parameter(property="defaultTupleSize")
  private Integer defaultTupleSize;

  @Parameter(readonly=true,defaultValue="${basedir}")
  private File baseDir_;

  @Parameter(readonly=true,defaultValue="${project.build.directory}")
  private File targetDir_;

  private static final Logger logger_ = LoggerFactory.getLogger( TcasesMojo.class);
  }
