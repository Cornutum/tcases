//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.Characters;

import java.util.Arrays;
import java.util.Optional;
import java.util.stream.IntStream;
import static java.util.stream.Collectors.joining;

/**
 * Defines a set of email address values that can be used by a request.
 */
public class EmailDomain extends AbstractStringDomain
  {
  /**
   * Creates a new EmailDomain instance.
   */
  public EmailDomain()
    {
    this( Characters.ASCII);
    }
  
  /**
   * Creates a new EmailDomain instance.
   */
  public EmailDomain( int maxLength)
    {
    this( maxLength, Characters.ASCII);
    }
  
  /**
   * Creates a new EmailDomain instance.
   */
  public EmailDomain( Characters chars)
    {
    super( MAX_LOCAL + MAX_DOMAIN + 1, chars);

    allowedLocalPartChars_ =
      chars.filtered( localPartChars_)
      .orElseThrow(
        () -> new ValueDomainException( String.format( "%s doesn't allow any of the required characters for the local part of an email addresss", chars)));

    allowedDomainPartChars_ =
      chars.filtered( domainPartChars_)
      .orElseThrow(
        () -> new ValueDomainException( String.format( "%s doesn't allow any of the required characters for the domain part of an email addresss", chars)));
    }
  
  /**
   * Creates a new EmailDomain instance.
   */
  public EmailDomain( int maxLength, Characters chars)
    {
    this( chars);
    setLengthRange( MIN_LENGTH, maxLength);
    }

  /**
   * Defines the initial length range for values in this domain.
   */
  protected void initLengthRange()
    {
    setLengthRange( MIN_LENGTH, getMaxLength());
    }

  /**
   * Defines a constant length range for values in this domain.
   */
  public void setLengthRange( Integer length)
    {
    super.setLengthRange(
      Optional.ofNullable( length)
      .filter( l -> l >= MIN_LENGTH && l <= getMaxLength())
      .orElseThrow( () -> new ValueDomainException( String.format( "Invalid email length=%s -- must be in [%s,%s]", length, MIN_LENGTH, getMaxLength()))));
    }

  /**
   * Defines the length range for values in this domain.
   */
  public void setLengthRange( Integer min, Integer max)
    {
    super.setLengthRange(

      Optional.ofNullable( min)
      .filter( m -> m >= MIN_LENGTH)
      .orElseThrow( () -> new ValueDomainException( String.format( "Invalid email length range=[%s,%s] -- must be >= %s", min, max, MIN_LENGTH))),

      Optional.ofNullable( max)
      .map( m -> {
        return
          Optional.of( m)
          .filter( v -> v <= getMaxLength())
          .orElseThrow( () -> new ValueDomainException( String.format( "Invalid email length range=[%s,%s] -- must be <= %s", min, max, getMaxLength())));
        })
      .orElse( null));
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  protected DataValue<String> dataValueOf( String value)
    {
    return new EmailValue( value);
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( String value)
    {
    return
      super.contains( value)
      && isEmail( value);
    }

  /**
   * Returns true if the given value is a valid email address
   */
  public static boolean isEmail( String value)
    {
    String[] parts = value.split( "@");
    String[] localParts = parts.length == 2? parts[0].split( "\\.") : null;
    String[] domainParts = parts.length == 2? parts[1].split( "\\.") : null;

    return
      Optional.ofNullable( localParts)
      .filter( locals -> parts[0].length() > 0 && parts[0].length() <= MAX_LOCAL)
      .map( locals -> Arrays.stream( locals).allMatch( part -> isStringOf( part, localPartChars_)))
      .orElse( false)
      &&
      Optional.ofNullable( domainParts)
      .filter( domains -> parts[1].length() > 0 && parts[1].length() <= MAX_DOMAIN)
      .map( labels -> Arrays.stream( labels).allMatch( part -> part.length() <= MAX_LABEL && isStringOf( part, domainPartChars_)))
      .orElse( false);
    }

  /**
   * Returns a new random string of the given length for this domain.
   */
  protected String newValue( ResolverContext context, int length)
    {
    int partsLength = length - 5;
    int localLengthMax = Math.min( MAX_LOCAL, partsLength - 1);
    int localLengthMin = length - MAX_DOMAIN - 1;
    int localLength = Math.max( context.getRandom().nextInt( localLengthMax) + 1, localLengthMin);
    int domainLength = partsLength - localLength;

    return
      new StringBuilder()
      .append( randomPathOf( context, allowedLocalPartChars_, localLength, localLength))
      .append( "@")
      .append( randomPathOf( context, allowedDomainPartChars_, domainLength, MAX_LABEL))
      .append( ".")
      .append( topLevels_[ context.getRandom().nextInt( topLevels_.length)])
      .toString();
    }

  /**
   * Returns a path of the given length composed of dot-separated parts. Each part is
   * a random string composed of the given characters and no longer than the given <CODE>maxPartLength</CODE>.
   */
  private static String randomPathOf( ResolverContext context, String chars, int length, int maxPartLength)
    {
    double lengthd = length;
    int maxParts = (int) Math.floor( (lengthd + 1) / 2);
    int minParts = (int) Math.ceil( (lengthd + 1) / (maxPartLength + 1));
    int parts = minParts + context.getRandom().nextInt( maxParts - minParts + 1);
    int partLength = (int) Math.ceil( (lengthd - parts + 1) / parts);

    int tailLength;
    for( ; (tailLength = (parts - 1) * (partLength + 1)) >= length; parts--);
    int prefixLength = length - tailLength;

    return
      IntStream.range( 0, parts)
      .mapToObj( i -> randomStringOf( context, chars, i==0? prefixLength : partLength))
      .collect( joining( "."));
    }

  /**
   * Returns a random string of the given length composed of the given characters.
   */
  private static String randomStringOf( ResolverContext context, String chars, int length)
    {
    StringBuilder randomString = new StringBuilder();
    for( int i = 0; i < length; i++)
      {
      randomString.append( chars.charAt( context.getRandom().nextInt( chars.length())));
      }
    
    return randomString.toString();
    }

  /**
   * Returns true if the given value is composed of only the given characters.
   */
  private static boolean isStringOf( String value, String chars)
    {
    return
      IntStream.range( 0, value.length())
      .map( i -> value.charAt(i))
      .allMatch( c -> chars.indexOf( c) >= 0);
    }

  private final String allowedLocalPartChars_;
  private final String allowedDomainPartChars_;

  private static final int MIN_LENGTH = 7;
  private static final int MAX_LOCAL = 64;
  private static final int MAX_DOMAIN = 255;
  private static final int MAX_LABEL = 63;

  private static final String alpha_ = "abcdefghijklmnopqrstuvwxyz";
  private static final String digits_ = "0123456789";
  private static final String special_ = "!#$%&'*+-/=?^_`{|}~;";
  private static final String localPartChars_ = alpha_ + alpha_.toUpperCase() + digits_ + special_;
  private static final String domainPartChars_ = alpha_ + alpha_.toUpperCase() + digits_;
  private static final String[] topLevels_ = new String[]{ "org", "com", "net", "edu", "gov" };
}
