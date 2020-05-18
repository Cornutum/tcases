package org.cornutum.tcases.openapi.moco;


import org.junit.Test;

import com.github.dreamhead.moco.junit.MocoJunitRunner;
import static com.github.dreamhead.moco.Moco.*;
import org.junit.Rule;
import com.github.dreamhead.moco.RestServer;
import static com.github.dreamhead.moco.MocoRest.*;
// Test case dependencies

public class RequestTestDef6Test {

    private static RestServer mocoServer;

    static {
        mocoServer = restServer( 12306);
        mocoServer.response( "Your request has been received.");
    }

    @Rule
    public MocoJunitRunner runner = MocoJunitRunner.restRunner( mocoServer);
    // Test case declarations

    @Test
    public void getPosts_0() {
        // Given...
        // When testServer=http://localhost:12306...
        // Then...
    }

    @Test
    public void getPosts_1() {
        // Given...
        // When testServer=http://localhost:12306...
        // Then...
    }

    @Test
    public void getPosts_2() {
        // Given...
        // When testServer=http://localhost:12306...
        // Then...
    }

    @Test
    public void getPosts_3() {
        // Given...
        // When testServer=http://localhost:12306...
        // Then...
    }

    @Test
    public void getPosts_4() {
        // Given...
        // When testServer=http://localhost:12306...
        // Then...
    }

    @Test
    public void getPosts_5() {
        // Given...
        // When testServer=http://localhost:12306...
        // Then...
    }

    @Test
    public void getPosts_6() {
        // Given...
        // When testServer=http://localhost:12306...
        // Then...
    }
    // Test case closing
}
