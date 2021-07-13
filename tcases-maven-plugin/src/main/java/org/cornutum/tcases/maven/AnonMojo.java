package org.cornutum.tcases.maven;

import org.cornutum.tcases.anon.AnonCommand.Options;
import org.cornutum.tcases.anon.AnonCommand;
import static org.cornutum.tcases.CommandUtils.*;
import static org.cornutum.tcases.maven.MojoUtils.*;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.plexus.util.DirectoryScanner;
import org.codehaus.plexus.util.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Set;

/**
 * Runs the <A href="https://github.com/Cornutum/tcases/blob/master/How-To-Anonymize.md#how-to-anonymize-your-input-model">Tcases Anonymizer</A>.
 */
@Mojo(name="anon")
public class AnonMojo extends AbstractMojo
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
          File outDir = new File( outRootDir, inputPath);

          String projectName = getProjectName( inputDef);
          String outFile = getOutFile();
          if( outFile == null)
            {
            outFile = String.format( "anon-%s", FileUtils.filename( inputFile));
            }
          else if( (outFile = getProjectFile( projectName, outFile)) == null)
            {
            throw new IllegalArgumentException( "Invalid outFile pattern='" + getOutFile() + "'");
            }

          String genDef = getGenDef();
          if( genDef != null && (genDef = getProjectFile( projectName, genDef)) == null)
            {
            throw new IllegalArgumentException( "Invalid genDef pattern='" + getGenDef() + "'");
            }

          // Set options for this Tcases project.
          Options options = new Options();
          options.setInputDef( inputDef);
          options.setGenDef( genDef==null? null : new File( genDef));
          options.setOutFile( new File( outDir, outFile));
          options.setContentType( getContentType());

          // Anonymize the system input definition for this Tcases project.
          AnonCommand.run( options);
          }
        }
      }
    catch( Exception e)
      {
      throw new MojoExecutionException( "Can't anonymize system input definition", e);
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
   * Defines a set of patterns that match the system input definition files to be anonymized.
   * By default, Tcases uses the single pattern defined by the <B><CODE>inputDef</CODE></B> parameter.
   */
  @Parameter(property="inputDefs")
  private Set<String> inputDefs;

  /**
   * Defines a single pattern that matches the system input definition files to be anonymized,
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
   * Defines the path to the directory where anonymized output is written.
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
   * Defines the name for the generator definition file for an input definition file.
   * This file name may contain at most one "*" wildcard character, in which case
   * the "*" is replaced by the <EM>project name</EM> of the corresponding
   * input definition file &mdash; see the <B><CODE>inputDef</CODE></B> parameter for
   * details.
   * The default value is "*-Generators.xml" or "*-Generators.json" (depending on the <B><CODE>contentType</CODE></B>).
   */
  @Parameter(property="genDef")
  private String genDef;

  /**
   * Defines the name for the anonymized output file generated from an input definition file.
   * This file name may contain at most one "*" wildcard character, in which case
   * the "*" is replaced by the <EM>project name</EM> of the corresponding
   * input definition file &mdash; see the <B><CODE>inputDef</CODE></B> parameter for
   * details.
   * The default value is "anon-*-Input.xml" or "anon-*-Input.json" (depending on the <B><CODE>contentType</CODE></B>).
   */
  @Parameter(property="outFile")
  private String outFile;

  @Parameter(readonly=true,defaultValue="${basedir}")
  private File baseDir_;

  @Parameter(readonly=true,defaultValue="${project.build.directory}")
  private File targetDir_;

  private static final Logger logger_ = LoggerFactory.getLogger( TcasesMojo.class);
  }
