package gov.uspto;


import org.junit.Test;

import java.util.List;
import java.util.Map;
import static java.util.stream.Collectors.*;

import io.restassured.http.Header;
import io.restassured.response.Response;

import org.cornutum.tcases.openapi.test.ResponseValidator;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class DsApiTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void get() {
        Response response =
            given()
                .baseUri( forTestServer( "https://developer.uspto.gov/ds-api"))
                .relaxedHTTPSValidation()
            .when()
                .request( "GET", "/")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getDatasetVersionFields_DatasetDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "https://developer.uspto.gov/ds-api"))
                .relaxedHTTPSValidation()
                .pathParam( "version", "v1")
                .pathParam( "dataset", "oa_citations")
            .when()
                .request( "GET", "/{dataset}/{version}/fields")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/{dataset}/{version}/fields", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/{dataset}/{version}/fields", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postDatasetVersionRecords_VersionDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer( "https://developer.uspto.gov/ds-api"))
                .relaxedHTTPSValidation()
                .pathParam( "dataset", "oa_citations")
                .pathParam( "version", "v1")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "criteria", "*:*")
                .formParam( "start", "0")
                .formParam( "rows", "100")
                .formParam( "ern", "1018.7")
                .formParam( "xerwg", "-1002")
            .when()
                .request( "POST", "/{dataset}/{version}/records")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/{dataset}/{version}/records", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/{dataset}/{version}/records", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postDatasetVersionRecords_BodyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "https://developer.uspto.gov/ds-api"))
                .relaxedHTTPSValidation()
                .pathParam( "dataset", "oa_citations")
                .pathParam( "version", "v1")
            .when()
                .request( "POST", "/{dataset}/{version}/records")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/{dataset}/{version}/records", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/{dataset}/{version}/records", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postDatasetVersionRecords_BodyApplicationXWwwFormUrlencodedValuePropertiesStartDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer( "https://developer.uspto.gov/ds-api"))
                .relaxedHTTPSValidation()
                .pathParam( "dataset", "oa_citations")
                .pathParam( "version", "v1")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "criteria", "*:*")
            .when()
                .request( "POST", "/{dataset}/{version}/records")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/{dataset}/{version}/records", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/{dataset}/{version}/records", response.statusCode(), responseHeaders( response));
    }

    private static Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private static String forTestServer( String defaultUri) {
        String testServer = tcasesApiServer();
        return
            defaultUri == null || !testServer.isEmpty()
            ? testServer
            : defaultUri;
    }

    private static String tcasesApiServer() {
        String uri = System.getProperty( "tcasesApiServer");
        return uri == null? "" : uri.trim();
    }

    private static Map<String,List<String>> responseHeaders( Response response) {
        return
            response.getHeaders().asList().stream()
            .collect( groupingBy( Header::getName, mapping( Header::getValue, toList())));
    }
}
