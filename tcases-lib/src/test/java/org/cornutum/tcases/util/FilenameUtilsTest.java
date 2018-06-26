package org.cornutum.tcases.util;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.*;

public class FilenameUtilsTest
  {

  @Test
  public void testGetBaseName()
    {
    assertNull(FilenameUtils.getBaseName( null));
    assertThat(FilenameUtils.getBaseName( ""), equalTo(""));
    assertThat(FilenameUtils.getBaseName( "."), equalTo(""));
    assertThat(FilenameUtils.getBaseName( "/"), equalTo(""));
    assertThat(FilenameUtils.getBaseName( "\\"), equalTo(""));

    assertThat(FilenameUtils.getBaseName( "foo"), equalTo("foo"));
    assertThat(FilenameUtils.getBaseName( "foo."), equalTo("foo"));
    assertThat(FilenameUtils.getBaseName( "/foo"), equalTo("foo"));
    assertThat(FilenameUtils.getBaseName( "\\foo"), equalTo("foo"));

    assertThat(FilenameUtils.getBaseName( "bim.bam/foo"), equalTo("foo"));
    assertThat(FilenameUtils.getBaseName( "bim.bam\\foo"), equalTo("foo"));

    assertThat(FilenameUtils.getBaseName( "foo.bar.txt"), equalTo("foo.bar"));
    assertThat(FilenameUtils.getBaseName( ".foo.bar.txt"), equalTo(".foo.bar"));
    assertThat(FilenameUtils.getBaseName( "/foo.bar.txt"), equalTo("foo.bar"));
    assertThat(FilenameUtils.getBaseName( "\\foo.bar.txt"), equalTo("foo.bar"));

    assertThat(FilenameUtils.getBaseName( "bim.bam/foo.bar.txt"), equalTo("foo.bar"));
    assertThat(FilenameUtils.getBaseName( "bim.bam\\foo.bar.txt"), equalTo("foo.bar"));
    }
  }
