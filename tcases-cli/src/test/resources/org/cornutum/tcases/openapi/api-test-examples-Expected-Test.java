package org.cornutum.examples;


import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class ExamplesTest {

    @Test
    public void postExamples_Param0Defined_Is_Yes() {
        given()
            .queryParam( "param0", "0.0")
        .when()
            .request( "POST", "/examples")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postExamples_Param0AlternativeUsed_Is_1() {
        given()
            .queryParam( "param0", "Howdy")
        .when()
            .request( "POST", "/examples")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postExamples_Param0AlternativeUsed_Is_2() {
        given()
            .queryParam( "param0", "-1,0,1")
        .when()
            .request( "POST", "/examples")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postExamples_Param0AlternativeUsed_Is_3() {
        given()
            .queryParam( "param0", "42")
        .when()
            .request( "POST", "/examples")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postExamples_Param0Alternative1Value_Is_QuePasa() {
        given()
            .queryParam( "param0", "Que pasa?")
        .when()
            .request( "POST", "/examples")
        .then()
            .statusCode( isSuccess())
            ;
    }

    private Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }
}
