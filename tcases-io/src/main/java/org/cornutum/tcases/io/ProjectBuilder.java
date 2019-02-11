//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.SystemInputDef;
import org.cornutum.tcases.SystemTestDef;
import org.cornutum.tcases.generator.IGeneratorSet;

import java.net.URI;

/**
 * Builds {@link Project} instances.
 *
 */
public class ProjectBuilder
  {
  /**
   * Creates a new builder for a Project with the given system input definition.
   */
  public static ProjectBuilder with( SystemInputDef systemInputDef)
    {
    return new ProjectBuilder().systemInput( systemInputDef);
    }
  /**
   * Creates a new builder for a Project with the given reference to a system input definition.
   */
  public static ProjectBuilder with( URI systemInputRef)
    {
    return new ProjectBuilder().systemInputRef( systemInputRef);
    }
  
  /**
   * Creates a new builder for the given Project.
   */
  public static ProjectBuilder with( Project project)
    {
    return new ProjectBuilder( project);
    }


  /**
   * Creates a new ProjectBuilder object.
   */
  public ProjectBuilder()
    {
    start();
    }

  /**
   * Creates a new ProjectBuilder object.
   */
  public ProjectBuilder( Project project)
    {
    start( project);
    }

  /**
   * Returns the current project.
   */
  public Project build()
    {
    return project_;
    }

  /**
   * Starts building a new project.
   */
  public ProjectBuilder start()
    {
    return start( null);
    }

  /**
   * Starts building a new project.
   */
  public ProjectBuilder start( Project project)
    {
    project_ =
      project == null
      ? new Project()
      : project;
    
    return this;
    }

  /**
   * Changes the system input definition for the project.
   */
  public ProjectBuilder systemInput( SystemInputDef systemInputDef)
    {
    project_.setSystemInput( systemInputDef);
    return this;
    }

  /**
   * Changes the base test definition for the project.
   */
  public ProjectBuilder baseTests( SystemTestDef systemTestDef)
    {
    project_.setBaseTests( systemTestDef);
    return this;
    }

  /**
   * Changes the generator set for the project.
   */
  public ProjectBuilder generators( IGeneratorSet generatorSet)
    {
    project_.setGenerators( generatorSet);
    return this;
    }

  /**
   * Changes the base URI for the location of project elements.
   */
  public ProjectBuilder refBase( URI location)
    {
    project_.setBaseLocation( location);
    return this;
    }

  /**
   * Changes the location of the system input definition for the project.
   */
  public ProjectBuilder systemInputRef( URI location)
    {
    project_.setSystemInputLocation( location);
    return this;
    }

  /**
   * Changes the location of the base tests definition for the project.
   */
  public ProjectBuilder baseTestRef( URI location)
    {
    project_.setBaseTestsLocation( location);
    return this;
    }

  /**
   * Changes the location of the generator set for the project.
   */
  public ProjectBuilder generatorsRef( URI location)
    {
    project_.setGeneratorsLocation( location);
    return this;
    }

  Project project_;
  }

