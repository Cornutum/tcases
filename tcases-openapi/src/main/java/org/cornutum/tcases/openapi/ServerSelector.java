//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.util.ToString;

import java.util.List;
import java.util.Optional;
import java.util.stream.IntStream;
import static java.util.Collections.emptyList;

/**
 * Given a list of API server descriptions, returns the index of the member that matches specific criteria.
 */
public abstract class ServerSelector
  {
  /**
   * Returns the index of the matching API server description.
   */
  public Optional<Integer> select( List<String> serverDescriptions)
    {
    List<String> candidates = Optional.ofNullable( serverDescriptions).orElse( emptyList());
    return
      IntStream.range( 0, candidates.size())
      .mapToObj( Integer::valueOf)
      .filter( i -> matches( i, candidates.get(i)))
      .findFirst();
    }

  /**
   * Returns a {@link ServerSelector} that selects the server description at the given index.
   */
  public static ServerSelector atIndex( int index)
    {
    return new AtIndex( index);
    }

  /**
   * Returns a {@link ServerSelector} that selects the server description containing the given text.
   */
  public static ServerSelector containing( String text)
    {
    return new Containing( text);
    }

  /**
   * Returns true if the server description at the given index matches this selector.
   */
  protected abstract boolean matches( Integer index, String description);

  /**
   * A {@link ServerSelector} that selects the server description at the given index.
   */
  private static class AtIndex extends ServerSelector
    {
    /**
     * Creates a new AtIndex instance.
     */
    public AtIndex( int index)
      {
      index_ = index;
      }

    protected boolean matches( Integer index, String description)
      {
      return index == index_;
      }

    public String toString()
      {
      return
        ToString.getBuilder( this)
        .append( index_)
        .toString();
      }
    
    private final int index_;
    }

  /**
   * A {@link ServerSelector} that selects the server description containing the given text.
   */
  private static class Containing extends ServerSelector
    {
    /**
     * Creates a new Containing instance.
     */
    public Containing( String text)
      {
      text_ = text;
      }

    protected boolean matches( Integer index, String description)
      {
      return
        Optional.ofNullable( text_)
        .map( t -> Optional.ofNullable( description).map( d -> d.contains( t)).orElse( false))
        .orElse( false);
      }

    public String toString()
      {
      return
        ToString.getBuilder( this)
        .append( text_)
        .toString();
      }

    private final String text_;
    }

  }
