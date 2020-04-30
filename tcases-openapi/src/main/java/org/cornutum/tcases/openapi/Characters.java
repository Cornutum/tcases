//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.IntStream;

/**
 * Defines a set of characters allowed in a specified context.
 */
public interface Characters
  {
  /**
   * Return the name of this set of characters.
   */
  public String getName();

  /**
   * Returns true if and only if the given character is allowed.
   */
  public boolean allowed( char c);
  
  /**
   * Returns true if and only if every character in the given string is allowed.
   */
  public default boolean allowed( String value)
    {
    String chars = Objects.toString( value, "");
    return IntStream.range( 0, chars.length()).allMatch( i -> allowed( chars.charAt(i)));
    }

  /**
   * Returns the given string after removing any characters that are not allowed.
   * Returns {@link Optional#empty} if all characters were removed or if the given
   * string is null.
   */
  public default Optional<String> filtered( String value)
    {
    String chars = Objects.toString( value, "");

    StringBuilder filtered = new StringBuilder();
    IntStream.range( 0, chars.length())
      .mapToObj( i -> chars.charAt(i))
      .filter( c -> allowed( c))
      .forEach( c -> filtered.append( c));

    return
      value == null || (filtered.length() == 0 && !chars.isEmpty())
      ? Optional.empty()
      : Optional.of( filtered.toString());
    }

  public static final Any ANY = new Any();
  public static final Ascii ASCII = new Ascii();
  public static final CookieValue COOKIE_VALUE = new CookieValue();
  public static final Token TOKEN = new Token();
  
  /**
   * Base class for {@link Characters} implementations.
   */
  public abstract class Base implements Characters
    {
    public String toString()
      {
      return String.format( "Characters[%s]", getName());
      }
    }
  
  /**
   * Defines the set of all printable characters.
   */
  public class Any extends Base
    {
    /**
     * Return the name of this set of characters.
     */
    public String getName()
      {
      return "ANY";
      }
    
    /**
     * Returns true if and only if the given character is allowed.
     */
    public boolean allowed( char c)
      {
      return isPrintable( c);
      }

    /**
     * Return true if the given chararacter is printable.
     */
    public static boolean isPrintable( char c)
      {
      return
        c == ' '
        || !(Character.isSpaceChar( c) || notVisible_.contains( Character.getType( c))) ;
      }

    /**
     * Return true if the character with the given code point is printable.
     */
    public static boolean isPrintable( int codePoint)
      {
      return
        Character.toChars( codePoint)[0] == ' '
        || !(Character.isSpaceChar( codePoint) || notVisible_.contains( Character.getType( codePoint))) ;
      }

    private static final List<Integer> notVisible_ =
      Arrays.asList(
        (int) Character.CONTROL,
        (int) Character.SURROGATE,
        (int) Character.UNASSIGNED);
    }

  /**
   * Defines the set of all printable ASCII characters.
   */
  public class Ascii extends Base
    {
    /**
     * Return the name of this set of characters.
     */
    public String getName()
      {
      return "ASCII";
      }
    
    /**
     * Returns true if and only if the given character is allowed.
     */
    public boolean allowed( char c)
      {
      return ascii_.indexOf( c) >= 0;
      }

    /**
     * Return the sequence of all characters in this set.
     */
    public static String chars()
      {
      return ascii_;
      }

    private static String asciiChars()
      {
      StringBuilder asciiChars = new StringBuilder();
      IntStream.range( 0, 128).filter( Any::isPrintable).forEach( codePoint -> asciiChars.appendCodePoint( codePoint));
      return asciiChars.toString();
      }

    private static final String ascii_ = asciiChars();
    }

  /**
   * Defines the set of characters allowed in an <a href="https://tools.ietf.org/html/rfc6265#section-4.1.1">HTTP cookie value</a>.
   */
  public class CookieValue extends Base
    {
    /**
     * Return the name of this set of characters.
     */
    public String getName()
      {
      return "COOKIE_VALUE";
      }
    
    /**
     * Returns true if and only if the given character is allowed.
     */
    public boolean allowed( char c)
      {
      return cookieValue_.indexOf( c) >= 0;
      }

    private static final String excluded_ = " \",;\\";
    
    private static String cookieValueChars()
      {
      String ascii = Ascii.chars();
      StringBuilder cookieValueChars = new StringBuilder();
      IntStream.range( 0, ascii.length())
        .filter( i -> excluded_.indexOf( ascii.charAt(i)) < 0)
        .forEach( i -> cookieValueChars.append( ascii.charAt(i)));
      return cookieValueChars.toString();
      }

    private static final String cookieValue_ = cookieValueChars();
    }

  /**
   * Defines the set of characters allowed in an <a href="https://tools.ietf.org/html/rfc2616#section-2.2">HTTP token</a>.
   */
  public class Token extends Base
    {
    /**
     * Return the name of this set of characters.
     */
    public String getName()
      {
      return "TOKEN";
      }
    
    /**
     * Returns true if and only if the given character is allowed.
     */
    public boolean allowed( char c)
      {
      return token_.indexOf( c) >= 0;
      }

    private static final String separators_ = "()<>@,;:\\\"/[]?={}";
    
    private static String tokenChars()
      {
      String ascii = Ascii.chars();
      StringBuilder tokenChars = new StringBuilder();
      IntStream.range( 0, ascii.length())
        .mapToObj( i -> ascii.charAt(i))
        .filter( c -> separators_.indexOf( c) < 0)
        .forEach( c -> tokenChars.append( c));
      return tokenChars.toString();
      }

    private static final String token_ = tokenChars();
    }

  }
