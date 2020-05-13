//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.openapi.resolver.RequestCase;
import org.cornutum.tcases.openapi.resolver.RequestTestDef;
import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.Optional;
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
   * Returns the request test cases for this source.
   */
  public List<RequestCase> getRequestCases()
    {
    return getTestDef().getRequestCases( getPaths(), getOperations());
    }

  public String toString()
    {
    ToStringBuilder builder = ToString.getBuilder( this);

    builder.append( getTestDef());
    Optional.ofNullable( getOperations()).ifPresent( ops -> builder.append( ops));
    Optional.ofNullable( getPaths()).ifPresent( paths -> builder.append( paths));
    
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
