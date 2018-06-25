package semaforo;

import java.util.Random;
import java.util.concurrent.atomic.*;

public class Test {

	public static void main(String[] args) throws InterruptedException {
		int aux = 5;
		AtomicInteger aux2 = new AtomicInteger(10);
		
		System.out.println(aux2.getAndAdd(aux));
	}


}
