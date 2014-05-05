package org.cornutum.tcases.maven;

import org.cornutum.tcases.Tcases;
import org.cornutum.tcases.Tcases.Options;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.plexus.util.DirectoryScanner;
import org.codehaus.plexus.util.FileUtils;

import java.io.File;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Implements the Tcases Maven plugin. For full details on Tcases &mdash; what it does and how it works &mdash; see
 * <A href="http://www.cornutum.org/tcases/docs/Tcases-Guide.htm">Tcases: The Complete Guide</A> at www.cornutum.org.
 *
 */
@Mojo(name="tcases",defaultPhase=LifecyclePhase.GENERATE_TEST_RESOURCES)
public class TcasesMojo extends AbstractMojo
  {
  public void execute() throws MojoExecutionException
    {
    try
      {
      // Gather input definition files
      DirectoryScanner inputScanner = new DirectoryScanner();
      if( getInputDefs().isEmpty())
        {
        getInputDefs().add( getInputDef());
        }
      inputScanner.setIncludes( getInputDefs().toArray( new String[0]));

      File inputRootDir = getInputDirFile();
      inputScanner.setBasedir( inputRootDir);
      inputScanner.scan();

      // Generate test cases for each input definition file.
      Tcases tcases = new Tcases();
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

        String projectName = Tcases.getProjectName( inputDef);
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
        options.setJUnit( isJUnit());
        options.setExtended( !isNewTests());
        options.setRandomSeed( getSeed());
        options.setDefaultTupleSize( getTuples());

        File transformDef = getTransformDefFile();
        if( transformDef != null)
          {
          if( !transformDef.isAbsolute())
            {
            String projectTransformDef = getProjectFile( projectName, transformDef.getPath());
            if( projectTransformDef == null)
              {
              throw new IllegalArgumentException( "Invalid transformDef pattern='" + transformDef + "'");
              }
            transformDef = new File( inputDir, projectTransformDef);
            }
          options.setTransformDef( transformDef);
          }
        options.setTransformParams( getTransformParams());

        // Generate test cases for this Tcases project.
        tcases.run( options);
        }
      }
    catch( Exception e)
      {
      throw new MojoExecutionException( "Can't generate test cases", e);
      }
    }

  /**
   * Returns the file name defined by applying the given pattern to the project name.
   * Returns null if the file pattern is invalid.
   */
  private String getProjectFile( String projectName, String filePattern)
    {
    String projectFile = null;
    if( !StringUtils.isBlank( filePattern))
      {
      Matcher matcher = projectFilePattern_.matcher( filePattern);
      if( matcher.matches())
        {
        projectFile =
          StringUtils.isBlank( matcher.group(2))
          ? filePattern
          : matcher.group(1) + projectName + matcher.group(3);
        }
      }
    
    return projectFile;
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
   * Changes the system input definition paths.
   */
  public void setInputDef( String inputDef)
    {
    this.inputDef = inputDef;
    }

  /**
   * Returns the system input definition paths.
   */
  public String getInputDef()
    {
    return inputDef;
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
  public void setJUnit( boolean junit)
    {
    this.junit = junit;
    }

  /**
   * Returns if using the JUnit transform.
   */
  public boolean isJUnit()
    {
    return junit;
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
   * By default, Tcases uses the single pattern defined by the <CODE>inputDef</CODE> parameter.
   */
  @Parameter(property="inputDefs")
  private Set<String> inputDefs;

  /**
   * Defines a single pattern that matches the system input definition files read by Tcases,
   * relative to the directory specified by the <CODE>inputDir</CODE>.
   * <P/>
   * An input definition file defines a <EM>project name</EM> that is used to form the
   * default names for other associated files. For an input definition file of the form
   * "${prefix}-Input.xml", the project name is "${prefix}". Otherwise, the project name
   * is the basename of the input definition file.
   */
  @Parameter(property="inputDef",defaultValue="**/*-Input.xml")
  private String inputDef;

  /**
   * Defines the path to the directory where system input definition files are located.
   * A relative path is applied relative to the <CODE>${basedir}</CODE> of
   * the project.
   */
  @Parameter(property="inputDir",defaultValue="${basedir}/src/test/tcases")
  private String inputDir;

  /**
   * Defines the path to the directory where Tcases output is written.
   * A relative path is applied relative to the <CODE>${project.build.directory}</CODE> of
   * the project.
   */
  @Parameter(property="outDir",defaultValue="${project.build.directory}/tcases")
  private String outDir;

  /**
   * Defines the name for the output file generated by Tcases from an input definition file.
   * This file name may contain at most one "*" wildcard character, in which case
   * the "*" is replaced by the <EM>project name</EM> of the corresponding
   * input definition file &mdash; see the <CODE>inputDef</CODE> parameter for
   * details.
   * The default value is "*-Test.xml" or, if the <CODE>junit</CODE> parameter is true,
   * "*Test.java";
   */
  @Parameter(property="outFile")
  private String outFile;

  /**
   * Defines the name for the initial test case definition files read by Tcases for an input definition file.
   * This file name may contain at most one "*" wildcard character, in which case
   * the "*" is replaced by the <EM>project name</EM> of the corresponding
   * input definition file &mdash; see the <CODE>inputDef</CODE> parameter for
   * details.
   * The default value is "*-Test.xml".
   */
  @Parameter(property="testDef")
  private String testDef;

  /**
   * Defines the name for the generator definition file read by Tcases for an input definition file.
   * This file name may contain at most one "*" wildcard character, in which case
   * the "*" is replaced by the <EM>project name</EM> of the corresponding
   * input definition file &mdash; see the <CODE>inputDef</CODE> parameter for
   * details.
   * The default value is "*-Generators.xml".
   */
  @Parameter(property="genDef")
  private String genDef;

  /**
   * Defines the path to the transform file applied by Tcases to the output for an input definition file.
   * A relative path is applied relative to the directory containing the corresponding input
   * definition file. Otherwise, a path with a leading "/" is applied relative to the
   * directory defined by the <CODE>inputDir</CODE> parameter.
   * This path name may contain at most one "*" wildcard character, in which case the "*"
   * is replaced by the <EM>project name</EM> of the corresponding input definition file &mdash;
   * see the <CODE>inputDef</CODE> parameter for details.
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
  @Parameter(property="jUnit",defaultValue="false")
  private boolean junit;

  /**
   * If true, ignore any initial test definitions. Otherwise, generate new test definitions
   * that extend the initial test definitions found in the file defined by the <CODE>testDef</CODE>
   * parameter.
   */
  @Parameter(property="newTests",defaultValue="false")
  private boolean newTests;

  /**
   * If defined, use the given random number for all generators. This updates the generator definitions specified
   * by the <CODE>genDef</CODE> parameter.
   */
  @Parameter(property="seed")
  private Long seed;

  /**
   * If defined, use the given default tuple size for all generators. This updates the generator definitions specified
   * by the <CODE>genDef</CODE> parameter.
   */
  @Parameter(property="defaultTupleSize")
  private Integer defaultTupleSize;

  @Parameter(readonly=true,defaultValue="${basedir}")
  private File baseDir_;

  @Parameter(readonly=true,defaultValue="${project.build.directory}")
  private File targetDir_;

  private static final Pattern projectFilePattern_ = Pattern.compile( "([^\\*]*)(\\*?)([^\\*]*)");
  }
