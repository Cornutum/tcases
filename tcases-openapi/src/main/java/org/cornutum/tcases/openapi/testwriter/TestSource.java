//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.openapi.resolver.RequestCase;
import org.cornutum.tcases.openapi.resolver.RequestTestDef;
import org.cornutum.tcases.openapi.test.ResponsesDef;
import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toSet;

/**
 * Defines the source for input to a {@link TestWriter}.
 */
public class TestSource
  {
  /**
   * Creates a new TestSource instance.
   */
  public TestSource( RequestTestDef testDef)
    {
    testDef_ = testDef;
    }

  /**
   * Returns the request test definition for this source.
   */
  public RequestTestDef getTestDef()
    {
    return testDef_;
    }

  /**
   * Returns the API name for this source.
   */
  public String getApi()
    {
    return getTestDef().getApi();
    }

  /**
   * Changes the request paths for this source.
   */
  public void setPaths( Iterable<String> paths)
    {
    paths_ =
      Optional.ofNullable( toStream( paths))
      .map( s -> s.collect( toSet()))
      .orElse( null);
    }

  /**
   * Changes the request paths for this source.
   */
  public void setPaths( String... paths)
    {
    setPaths( paths.length == 0 ? null : Arrays.asList( paths));
    }

  /**
   * Returns the request paths for this source or null if all paths are used.
   */
  public Set<String> getPaths()
    {
    return
      Optional.ofNullable( paths_)
      .map( Collections::unmodifiableSet)
      .orElse( null);
    }

  /**
   * Changes the request operations for this source.
   */
  public void setOperations( Iterable<String> operations)
    {
    ops_ =
      Optional.ofNullable( toStream( operations))
      .map( s -> s.collect( toSet()))
      .orElse( null);
    }

  /**
   * Changes the request operations for this source.
   */
  public void setOperations( String... operations)
    {
    setOperations( operations.length == 0 ? null : Arrays.asList( operations));
    }

  /**
   * Returns the request operations for this source or null if all operations are used.
   */
  public Set<String> getOperations()
    {
    return
      Optional.ofNullable( ops_)
      .map( Collections::unmodifiableSet)
      .orElse( null);
    }

  /**
   * Changes if success test cases are included in this source.
   */
  public void setSuccessIncluded( boolean included)
    {
    if( !included && !isFailureIncluded())
      {
      throw new IllegalStateException( "Must include either success or failure cases");
      }
    includeSuccess_ = included;
    }

  /**
   * Returns if success test cases are included in this source.
   */
  public boolean isSuccessIncluded()
    {
    return includeSuccess_;
    }

  /**
   * Changes if failure test cases are included in this source.
   */
  public void setFailureIncluded( boolean included)
    {
    if( !included && !isSuccessIncluded())
      {
      throw new IllegalStateException( "Must include either success or failure cases");
      }
    includeFailure_ = included;
    }

  /**
   * Returns if failure test cases are included in this source.
   */
  public boolean isFailureIncluded()
    {
    return includeFailure_;
    }

  /**
   * Returns the request test cases for this source.
   */
  public List<RequestCase> getRequestCases()
    {
    return
      getTestDef().getRequestCases( getPaths(), getOperations())
      .stream()
      .filter( rc -> rc.isFailure()? isFailureIncluded() : isSuccessIncluded())
      .collect( toList());
    }

  /**
   * Changes the request response definitions for this source.
   */
  public void setResponses( ResponsesDef responses)
    {
    responses_ = responses;
    }

  /**
   * Returns the request response definitions for this source.
   */
  public ResponsesDef getResponses()
    {
    return
      Optional.ofNullable( responses_)
      .map( responses -> responses.forPaths( getPaths()).forOps( getOperations()))
      .orElse( null);
    }

  @Override
  public String toString()
    {
    ToStringBuilder builder = ToString.getBuilder( this);

    builder.append( getTestDef());
    Optional.ofNullable( getOperations()).ifPresent( ops -> builder.append( ops));
    Optional.ofNullable( getPaths()).ifPresent( paths -> builder.append( paths));
    if( !isSuccessIncluded())
      {
      builder.append( "success", false);
      }
    if( !isFailureIncluded())
      {
      builder.append( "failure", false);
      }
    
    return builder.toString();    
    }

  /**
   * Returns a new {@link Builder}.
   */
  public static Builder from( RequestTestDef testDef)
    {
    return new Builder( testDef);
    }
  
  private RequestTestDef testDef_;
  private Set<String> paths_;
  private Set<String> ops_;
  private ResponsesDef responses_;
  private boolean includeSuccess_ = true;
  private boolean includeFailure_ = true;

  /**
   * Builds a {@link TestSource} instance.
   */
  public static class Builder
    {
    /**
     * Creates a new {@link Builder}
     */
    private Builder( RequestTestDef testDef)
      {
      source_ = new TestSource( testDef);
      }

    public Builder paths( String... paths)
      {
      source_.setPaths( paths);
      return this;
      }

    public Builder operations( String... operations)
      {
      source_.setOperations( operations);
      return this;
      }

    public Builder includeSuccess( boolean include)
      {
      source_.setSuccessIncluded( include);
      return this;
      }

    public Builder includeFailure( boolean include)
      {
      source_.setFailureIncluded( include);
      return this;
      }

    public Builder responses( ResponsesDef responses)
      {
      source_.setResponses( responses);
      return this;
      }
    
    /**
     * Returns the {@link TestSource} instance for this builder.
     */
    public TestSource build()
      {
      return source_;
      }

    private TestSource source_;
    }
  }
